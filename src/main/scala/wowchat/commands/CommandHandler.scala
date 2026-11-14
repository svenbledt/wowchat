package wowchat.commands

import com.typesafe.scalalogging.StrictLogging
import net.dv8tion.jda.api.entities.channel.middleman.MessageChannel
import wowchat.common.Global
import net.dv8tion.jda.api.components.container.Container
import net.dv8tion.jda.api.events.interaction.command.SlashCommandInteractionEvent
import net.dv8tion.jda.api.interactions.commands.OptionType
import net.dv8tion.jda.api.interactions.commands.build.{Commands, OptionData, SlashCommandData}
import net.dv8tion.jda.api.utils.messages.MessageCreateBuilder
import wowchat.discord.{Discord, WhoMessages}
import wowchat.game.{GamePackets, GuildInfo, GuildMember}

import scala.collection.mutable
import scala.util.Try

// reply delivers the response; it either posts to a channel (?who) or answers a slash command (/who)
case class WhoRequest(reply: Iterable[Container] => Unit, playerName: String)
case class WhoResponse(playerName: String, guildName: String, lvl: Int, cls: String, race: String, gender: Option[String], zone: String)

object CommandHandler extends StrictLogging {

  private val NOT_ONLINE = "Bot is not online."
  private val MAX_NAME_LENGTH = 16

  // make some of these configurable
  private val trigger = "?"

  // gross. rewrite
  var whoRequest: WhoRequest = _

  // Slash command definitions, registered per Discord server on connect
  val slashCommands: Seq[SlashCommandData] = Seq(
    Commands.slash("who", "Look up a player, or list online guild members when no name is given")
      .addOptions(new OptionData(OptionType.STRING, "name", "Character name", false).setMaxLength(MAX_NAME_LENGTH)),
    Commands.slash("online", "List online guild members"),
    Commands.slash("gmotd", "Show the guild message of the day")
  )

  def isCommandChannel(channelName: String): Boolean = {
    val enableCommandsChannels = Global.config.discord.enableCommandsChannels
    enableCommandsChannels.isEmpty || enableCommandsChannels.contains(channelName.toLowerCase)
  }

  def handleSlashCommand(event: SlashCommandInteractionEvent): Unit = {
    if (!isCommandChannel(event.getChannel.getName)) {
      event.reply("Commands are not enabled in this channel.").setEphemeral(true).queue()
      return
    }

    Global.game match {
      case None =>
        event.reply(NOT_ONLINE).setEphemeral(true).queue()
      case Some(game) =>
        // the who lookup is answered asynchronously by the game server, so always defer and reply via the hook
        event.deferReply().queue()
        val hook = event.getHook
        val reply: Iterable[Container] => Unit = containers => containers.foreach(container =>
          hook.sendMessage(new MessageCreateBuilder().useComponentsV2().setComponents(container).build).queue()
        )

        val playerName = Option(event.getOption("name")).map(_.getAsString.trim).filter(_.nonEmpty)
        event.getName match {
          case "who" if playerName.isDefined =>
            whoRequest = WhoRequest(reply, playerName.get)
            game.handleWho(playerName)
          case "who" | "online" =>
            reply(game.buildGuildiesOnlineMessages)
          case "gmotd" =>
            hook.sendMessage(game.handleGmotd().getOrElse("No guild message of the day is set.")).queue()
          case other =>
            hook.sendMessage(s"Unknown command /$other").queue()
        }
    }
  }

  // returns back the message as an option if unhandled
  // needs to be refactored into a Map[String, <Intelligent Command Handler Function>]
  def apply(fromChannel: MessageChannel, message: String): Boolean = {
    if (!message.startsWith(trigger)) {
      return false
    }

    val splt = message.substring(trigger.length).split(" ")
    val possibleCommand = splt(0).toLowerCase
    val arguments = if (splt.length > 1 && splt(1).length <= MAX_NAME_LENGTH) Some(splt(1)) else None

    Try {
      possibleCommand match {
        case "who" | "online" =>
          Global.game.fold({
            Discord.sendMessage(fromChannel, NOT_ONLINE)
            return true
          })(game => {
            arguments match {
              case Some(playerName) =>
                // set before querying so the SMSG_WHO response can't arrive before the request is known
                whoRequest = WhoRequest(containers => Discord.sendComponents(fromChannel, containers), playerName)
                game.handleWho(arguments)
              case None =>
                Discord.sendComponents(fromChannel, game.buildGuildiesOnlineMessages)
                None
            }
          })
        case "gmotd" =>
          Global.game.fold({
            Discord.sendMessage(fromChannel, NOT_ONLINE)
            return true
          })(_.handleGmotd())
      }
    }.fold(throwable => {
      // command not found, should send to wow chat
      false
    }, opt => {
      // command found, do not send to wow chat
      if (opt.isDefined) {
        Discord.sendMessage(fromChannel, opt.get)
      }
      true
    })
  }

  // eww
  def handleWhoResponse(whoResponse: Option[WhoResponse],
                        guildInfo: GuildInfo,
                        guildRoster: mutable.Map[Long, GuildMember],
                        guildRosterMatcherFunc: GuildMember => Boolean): Iterable[Container] = {
    whoResponse.map(r => {
      Seq(WhoMessages.onlinePlayer(r))
    }).getOrElse({
      // Check guild roster
      // Some servers don't set guild guid in character selection packet, so the guild name can be unknown.
      // The only other way to get this information is through parsing SMSG_UPDATE_OBJECT
      // and its compressed version which is quite annoying especially across expansions.
      val guildName = Option(guildInfo).map(_.name)
      guildRoster
        .values
        .filter(guildRosterMatcherFunc)
        .map(guildMember => {
          val cls = new GamePackets{}.Classes.valueOf(guildMember.charClass) // ... should really move that out
          WhoMessages.offlineGuildMember(guildMember, cls, guildName)
        })
    })
  }
}
