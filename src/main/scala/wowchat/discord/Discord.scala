package wowchat.discord

import wowchat.commands.CommandHandler
import wowchat.common._
import com.typesafe.scalalogging.StrictLogging
import com.vdurmont.emoji.EmojiParser
import net.dv8tion.jda.api.JDABuilder
import net.dv8tion.jda.api.JDA.Status
import net.dv8tion.jda.api.entities.{Activity, MessageType}
import net.dv8tion.jda.api.entities.Activity.ActivityType
import net.dv8tion.jda.api.entities.channel.ChannelType
import net.dv8tion.jda.api.entities.channel.middleman.MessageChannel
import net.dv8tion.jda.api.events.StatusChangeEvent
import net.dv8tion.jda.api.events.message.MessageReceivedEvent
import net.dv8tion.jda.api.events.session.ShutdownEvent
import net.dv8tion.jda.api.hooks.ListenerAdapter
import net.dv8tion.jda.api.requests.{CloseCode, GatewayIntent}
import net.dv8tion.jda.api.utils.MemberCachePolicy
import net.dv8tion.jda.api.utils.cache.CacheFlag
import wowchat.game.GamePackets

import scala.collection.JavaConverters._
import scala.collection.mutable

object Discord {

  def sendMessage(channel: MessageChannel, message: String): Unit = {
    splitUpByLength(message, 2000).foreach(channel.sendMessage(_).queue)
  }

  private def splitUpByLength(message: String, maxLength: Int): Seq[String] = {
    val retArr = mutable.ArrayBuffer.empty[String]

    var tmp = message
    while (tmp.length > maxLength) {
      val subStr = tmp.substring(0, maxLength)
      val spaceIndex = subStr.lastIndexOf(' ')
      tmp = if (spaceIndex == -1) {
        retArr += subStr
        tmp.substring(maxLength)
      } else {
        retArr += subStr.substring(0, spaceIndex)
        tmp.substring(spaceIndex + 1)
      }
    }

    if (tmp.nonEmpty) {
      retArr += tmp
    }

    retArr
  }

  private def splitUpMessageToWow(format: String, name: String, message: String): Seq[String] = {
    val maxTmpLen = 255 - format
      .replace("%time", Global.getTime)
      .replace("%user", name)
      .replace("%message", "")
      .length

    splitUpByLength(message, maxTmpLen)
      .map(message => {
        val formatted = format
          .replace("%time", Global.getTime)
          .replace("%user", name)
          .replace("%message", message)

        // If the final formatted message is a dot command, it should be disabled. Add a space in front.
        if (formatted.startsWith(".")) {
          s" $formatted"
        } else {
          formatted
        }
      })
  }
}

class Discord(discordConnectionCallback: CommonConnectionCallback) extends ListenerAdapter
  with GamePackets with StrictLogging {

  private val jda = JDABuilder
    .createDefault(Global.config.discord.token,
      GatewayIntent.GUILD_EXPRESSIONS,
      GatewayIntent.GUILD_MEMBERS,
      GatewayIntent.GUILD_MESSAGES,
      GatewayIntent.GUILD_PRESENCES,
      GatewayIntent.MESSAGE_CONTENT)
    .setMemberCachePolicy(MemberCachePolicy.ALL)
    .disableCache(CacheFlag.SCHEDULED_EVENTS,
      CacheFlag.VOICE_STATE)
    .addEventListeners(this)
    .build

  private val messageResolver = MessageResolver(jda)

  private var lastStatus: Option[Activity] = None
  private var firstConnect = true

  def changeStatus(gameType: ActivityType, message: String): Unit = {
    lastStatus = Some(Activity.of(gameType, message))
    jda.getPresence.setActivity(lastStatus.get)
  }

  def changeGuildStatus(message: String): Unit = {
    changeStatus(ActivityType.WATCHING, message)
  }

  def changeRealmStatus(message: String): Unit = {
    changeStatus(ActivityType.CUSTOM_STATUS, message)
  }

  def syncGuildRoles(guildRoster: Map[Long, wowchat.game.GuildMember]): Unit = {
    Global.config.guildConfig.roleSyncConfig.foreach { roleSyncConfig =>
      // Only sync roles if enabled
      if (!roleSyncConfig.enabled) {
        return
      }

      roleSyncConfig.roleId.foreach { roleIdStr =>
        try {
          val pattern = roleSyncConfig.pattern.r
          val guilds = jda.getGuilds.asScala

          guilds.foreach { guild =>
            val role = guild.getRoleById(roleIdStr)
            if (role == null) {
              logger.warn(s"Role with ID $roleIdStr not found in Discord guild ${guild.getName}")
              return
            }

            val discordMembers = guild.getMembers.asScala
            // Keep track of Discord users who should have the role based on officer notes
            val discordUsersWithRole = mutable.Set.empty[String]

            // Add roles to Discord members found in officer notes
            guildRoster.values.foreach { member =>
              if (member.officerNote.nonEmpty) {
                try {
                  // Try all matches in the officer note, not just the first one
                  val matches = pattern.findAllMatchIn(member.officerNote)
                  var foundMatch = false

                  matches.foreach { m =>
                    if (!foundMatch) {
                      // Get the username from group 1
                      val discordUsername = if (m.groupCount >= 1 && m.group(1) != null) {
                        m.group(1).stripPrefix("@").toLowerCase
                      } else {
                        ""
                      }

                      if (discordUsername.nonEmpty) {
                        // Find Discord member by username
                        discordMembers.find(_.getUser.getName.toLowerCase == discordUsername).foreach { discordMember =>
                          foundMatch = true
                          val userName = discordMember.getUser.getName
                          discordUsersWithRole += discordUsername
                          // Check if member already has the role
                          if (!discordMember.getRoles.asScala.exists(_.getId == roleIdStr)) {
                            guild.addRoleToMember(discordMember, role).queue(
                              _ => logger.info(s"Assigned role ${role.getName} to Discord user $userName (matched with WoW character ${member.name})"),
                              error => logger.error(s"Failed to assign role to $userName: ${error.getMessage}")
                            )
                          }
                        }
                      }
                    }
                  }
                } catch {
                  case e: Exception =>
                    logger.error(s"Error processing officer note for ${member.name}: ${e.getMessage}")
                }
              }
            }

            // Remove role from Discord members who don't have their tag in any officer note
            discordMembers.foreach { discordMember =>
              val userName = discordMember.getUser.getName.toLowerCase
              val hasRole = discordMember.getRoles.asScala.exists(_.getId == roleIdStr)
              
              if (hasRole && !discordUsersWithRole.contains(userName)) {
                guild.removeRoleFromMember(discordMember, role).queue(
                  _ => logger.info(s"Removed role ${role.getName} from Discord user ${discordMember.getUser.getName} (not found in officer notes)"),
                  error => logger.error(s"Failed to remove role from ${discordMember.getUser.getName}: ${error.getMessage}")
                )
              }
            }
          }
        } catch {
          case e: Exception =>
            logger.error(s"Error in syncGuildRoles: ${e.getMessage}", e)
        }
      }
    }
  }

  def sendMessageFromWow(from: Option[String], message: String, wowType: Byte, wowChannel: Option[String]): Unit = {
    Global.wowToDiscord.get((wowType, wowChannel.map(_.toLowerCase))).foreach(discordChannels => {
      val parsedLinks = messageResolver.resolveEmojis(messageResolver.stripColorCoding(messageResolver.resolveLinks(message)))

      discordChannels.foreach {
        case (channel, channelConfig) =>
          val errors = mutable.ArrayBuffer.empty[String]
          val parsedResolvedTags = from.map(_ => {
            messageResolver.resolveTags(channel, parsedLinks, errors += _)
          })
            .getOrElse(parsedLinks)
            .replace("`", "\\`")
            .replace("*", "\\*")
            .replace("_", "\\_")
            .replace("~", "\\~")

          val formatted = channelConfig
            .format
            .replace("%time", Global.getTime)
            .replace("%user", from.getOrElse(""))
            .replace("%message", parsedResolvedTags)
            .replace("%target", wowChannel.getOrElse(""))

          val filter = shouldFilter(channelConfig.filters, formatted)
          logger.info(s"${if (filter) "FILTERED " else ""}WoW->Discord(${channel.getName}) $formatted")
          if (!filter) {
            Discord.sendMessage(channel, formatted)
          }
          if (Global.config.discord.enableTagFailedNotifications) {
            errors.foreach(error => {
              Global.game.foreach(_.sendMessageToWow(ChatEvents.CHAT_MSG_WHISPER, error, from))
              Discord.sendMessage(channel, error)
            })
          }
      }
    })
  }

  def sendGuildNotification(eventKey: String, message: String): Unit = {
    Global.guildEventsToDiscord
      .getOrElse(eventKey, Global.wowToDiscord.getOrElse(
          (ChatEvents.CHAT_MSG_GUILD, None), mutable.Set.empty
        ).map(_._1)
      )
      .foreach(channel => {
        logger.info(s"WoW->Discord(${channel.getName}) $message")
        Discord.sendMessage(channel, message)
      })
  }

  def sendAchievementNotification(name: String, achievementId: Int): Unit = {
    val notificationConfig = Global.config.guildConfig.notificationConfigs("achievement")
    if (!notificationConfig.enabled) {
      return
    }

    Global.wowToDiscord.get((ChatEvents.CHAT_MSG_GUILD, None))
      .foreach(_.foreach {
        case (discordChannel, _) =>
          val formatted = notificationConfig
            .format
            .replace("%time", Global.getTime)
            .replace("%user", name)
            .replace("%achievement", messageResolver.resolveAchievementId(achievementId))

          Discord.sendMessage(discordChannel, formatted)
      })
  }

  override def onStatusChange(event: StatusChangeEvent): Unit = {
    event.getNewStatus match {
      case Status.CONNECTED =>
        lastStatus.foreach(game => changeStatus(game.getType, game.getName))
        // this is a race condition if already connected to wow, reconnect to discord, and bot tries to send
        // wow->discord message. alternatively it was throwing already garbage collected exceptions if trying
        // to use the previous connection's channel references. I guess need to refill these maps on discord reconnection
        Global.discordToWow.clear
        Global.wowToDiscord.clear
        Global.guildEventsToDiscord.clear

        // getNext seq of needed channels from config
        val configChannels = Global.config.channels.map(channelConfig => {
          channelConfig.discord.channel.toLowerCase -> channelConfig
        })
        val configChannelsNames = configChannels.map(_._1)

        val discordTextChannels = event.getEntity.getTextChannels.asScala
        val eligibleDiscordChannels = discordTextChannels
          .filter(channel =>
            configChannelsNames.contains(channel.getName.toLowerCase) ||
            configChannelsNames.contains(channel.getId)
          )

        // build directional maps
        eligibleDiscordChannels.foreach(channel => {
          configChannels
            .filter {
              case (name, _) =>
                name.equalsIgnoreCase(channel.getName) ||
                name == channel.getId
            }
            .foreach {
              case (name, channelConfig) =>
                if (channelConfig.chatDirection == ChatDirection.both ||
                  channelConfig.chatDirection == ChatDirection.discord_to_wow) {
                  Global.discordToWow.addBinding(
                    name.toLowerCase, channelConfig.wow
                  )
                }

                if (channelConfig.chatDirection == ChatDirection.both ||
                  channelConfig.chatDirection == ChatDirection.wow_to_discord) {
                  Global.wowToDiscord.addBinding(
                    (channelConfig.wow.tp, channelConfig.wow.channel.map(_.toLowerCase)),
                    (channel, channelConfig.discord)
                  )
                }
            }
          })

        // build guild notification maps
        val guildEventChannels = Global.config.guildConfig.notificationConfigs
          .filter {
            case (_, notificationConfig) =>
              notificationConfig.enabled
          }
          .flatMap {
            case (key, notificationConfig) =>
              notificationConfig.channel.map(key -> _)
          }

        discordTextChannels.foreach(channel => {
          guildEventChannels
            .filter {
              case (_, name) =>
                name.equalsIgnoreCase(channel.getName) ||
                name == channel.getId
            }
            .foreach {
              case (notificationKey, _) =>
                Global.guildEventsToDiscord.addBinding(notificationKey, channel)
            }
        })

        if (Global.discordToWow.nonEmpty || Global.wowToDiscord.nonEmpty) {
          if (firstConnect) {
            discordConnectionCallback.connected
            firstConnect = false
          } else {
            discordConnectionCallback.reconnected
          }
        } else {
          logger.error("No discord channels configured!")
        }
      case Status.DISCONNECTED =>
        discordConnectionCallback.disconnected
      case _ =>
    }
  }

  override def onShutdown(event: ShutdownEvent): Unit = {
    event.getCloseCode match {
      case CloseCode.DISALLOWED_INTENTS =>
        logger.error("Per new Discord rules, you must check the PRESENCE INTENT, SERVER MEMBERS INTENT, and MESSAGE CONTENT INTENT boxes under \"Privileged Gateway Intents\" for this bot in the developer portal. You can find more info at https://discord.com/developers/docs/topics/gateway#privileged-intents")
      case _ =>
    }
  }

  override def onMessageReceived(event: MessageReceivedEvent): Unit = {
    // ignore messages received from self
    if (event.getAuthor.getIdLong == jda.getSelfUser.getIdLong) {
      return
    }

    // ignore messages from non-text channels and non-guild channels
    if (event.getChannelType != ChannelType.TEXT) {
      return
    }
    
    // ignore messages from non-guild channels (e.g., DMs)
    if (!event.isFromGuild) {
      return
    }

    // ignore non-default messages
    val messageType = event.getMessage.getType
    if (messageType != MessageType.DEFAULT && messageType != MessageType.INLINE_REPLY) {
      return
    }

    val channel = event.getChannel
    val channelId = channel.getId
    val channelName = event.getChannel.getName.toLowerCase
    
    // Handle case where getMember() can be null (e.g., when member has left the guild)
    val member = event.getMember
    if (member == null) {
      logger.debug("Skipping message: member is null (possibly member left the guild)")
      return
    }
    
    val effectiveName = sanitizeName(member.getEffectiveName)
    val message = (sanitizeMessage(event.getMessage.getContentDisplay) +: event.getMessage.getAttachments.asScala.map(_.getUrl))
      .filter(_.nonEmpty)
      .mkString(" ")
    val enableCommandsChannels = Global.config.discord.enableCommandsChannels
    logger.debug(s"RECV DISCORD MESSAGE: [${channel.getName}] [$effectiveName]: $message")
    if (message.isEmpty) {
      logger.error(s"Received a message in channel ${channel.getName} but the content was empty. You likely forgot to enable MESSAGE CONTENT INTENT for your bot in the Discord Developers portal.")
    }

    if ((enableCommandsChannels.nonEmpty && !enableCommandsChannels.contains(channelName)) || !CommandHandler(channel, message)) {
      // send to all configured wow channels
      Global.discordToWow
        .get(channelName)
        .fold(Global.discordToWow.get(channelId))(Some(_))
        .foreach(_.foreach(channelConfig => {
          val finalMessages = if (shouldSendDirectly(message)) {
            Seq(message)
          } else {
            Discord.splitUpMessageToWow(channelConfig.format, effectiveName, message)
          }

          finalMessages.foreach(finalMessage => {
            val filter = shouldFilter(channelConfig.filters, finalMessage)
            logger.info(s"${if (filter) "FILTERED " else ""}Discord->WoW(${
              channelConfig.channel.getOrElse(ChatEvents.valueOf(channelConfig.tp))
            }) $finalMessage")
            if (!filter) {
              Global.game.fold(logger.error("Cannot send message! Not connected to WoW!"))(handler => {
                handler.sendMessageToWow(channelConfig.tp, finalMessage, channelConfig.channel)
              })
            }
          })
        }))
    }
  }

  def shouldSendDirectly(message: String): Boolean = {
    val discordConf = Global.config.discord
    val trimmed = message.drop(1).toLowerCase

    message.startsWith(".") &&
    discordConf.enableDotCommands &&
      (
        discordConf.dotCommandsWhitelist.isEmpty ||
        discordConf.dotCommandsWhitelist.contains(trimmed) ||
        // Theoretically it would be better to construct a prefix tree for this.
        !discordConf.dotCommandsWhitelist.forall(item => {
          if (item.endsWith("*")) {
            !trimmed.startsWith(item.dropRight(1).toLowerCase)
          } else {
            true
          }
        })
      )
  }

  def shouldFilter(filtersConfig: Option[FiltersConfig], message: String): Boolean = {
    filtersConfig
      .fold(Global.config.filters)(Some(_))
      .exists(filters => filters.enabled && filters.patterns.exists(message.matches))
  }

  def sanitizeName(name: String): String = {
    name.replace("|", "||")
  }

  def sanitizeMessage(message: String): String = {
    val withAliases = EmojiParser.parseToAliases(message, EmojiParser.FitzpatrickAction.REMOVE)
    convertEmojisToTextEmoticons(withAliases)
  }

  // Convert Discord emoji codes to text emoticons for in-game chat
  def convertEmojisToTextEmoticons(message: String): String = {
    val emojiMap = Map(
      // Happy/Positive emotions
      ":slightly_smiling:" -> ":)",
      ":smile:" -> ":D",
      ":grinning:" -> ":D",
      ":smiley:" -> ":)",
      ":laughing:" -> ":D",
      ":satisfied:" -> ":D",
      ":joy:" -> "xD",
      ":rofl:" -> "ROFL",
      ":relaxed:" -> ":)",
      ":blush:" -> "^^",
      ":innocent:" -> "O:)",
      ":smiling_face_with_3_hearts:" -> "<3",
      ":heart:" -> "<3",
      ":kissing_heart:" -> ":*",
      ":kissing:" -> ":*",
      ":wink:" -> ";)",
      ":stuck_out_tongue:" -> ":P",
      ":stuck_out_tongue_winking_eye:" -> ";P",
      ":stuck_out_tongue_closed_eyes:" -> "xP",
      ":zany_face:" -> ":P",
      ":yum:" -> ":P",
      
      // Sad/Negative emotions
      ":disappointed:" -> ":(",
      ":worried:" -> ":(",
      ":cry:" -> ":'(",
      ":sob:" -> "T_T",
      ":frowning:" -> ":(",
      ":anguished:" -> "D:",
      ":pleading_face:" -> ":(",
      ":frowning_face:" -> ":(",
      ":slightly_frowning_face:" -> ":(",
      ":confused:" -> ":/",
      ":upside_down_face:" -> "(:",
      ":money_mouth_face:" -> "$_$",
      
      // Surprise/Shock
      ":open_mouth:" -> ":O",
      ":hushed:" -> ":O",
      ":astonished:" -> ":O",
      ":flushed:" -> "O_O",
      ":scream:" -> "D:",
      ":fearful:" -> "D:",
      ":cold_sweat:" -> "o_O",
      ":disappointed_relieved:" -> "u_u",
      ":sweat:" -> "^^'",
      
      // Neutral/Other
      ":neutral_face:" -> ":|",
      ":expressionless:" -> "-_-",
      ":no_mouth:" -> ":-",
      ":thinking:" -> "o.O",
      ":thinking_face:" -> "o.O",
      ":face_with_raised_eyebrow:" -> "o.O",
      ":unamused:" -> "-_-",
      ":rolling_eyes:" -> "-_-",
      ":grimacing:" -> ":S",
      ":lying_face:" -> ":L",
      ":relieved:" -> "phew",
      ":pensive:" -> ":((",
      ":sleepy:" -> "zzz",
      ":sleeping:" -> "zzz",
      ":drooling_face:" -> ":P",
      ":sleeping:" -> "Zzz",
      
      // Cool/Sunglasses
      ":sunglasses:" -> "B)",
      ":smirk:" -> ";)",
      ":nerd_face:" -> "8)",
      
      // Angry
      ":angry:" -> ">:(",
      ":rage:" -> ">:O",
      ":face_with_symbols_on_mouth:" -> "@#$%!",
      ":triumph:" -> ">:(",
      
      // Love
      ":heart:" -> "<3",
      ":heartbeat:" -> "<3",
      ":heartpulse:" -> "<3",
      ":sparkling_heart:" -> "<3",
      ":two_hearts:" -> "<3<3",
      ":revolving_hearts:" -> "<3",
      ":cupid:" -> "<3",
      ":gift_heart:" -> "<3",
      ":broken_heart:" -> "</3",
      
      // Symbols
      ":thumbsup:" -> "+1",
      ":+1:" -> "+1",
      ":thumbsdown:" -> "-1",
      ":-1:" -> "-1",
      ":ok_hand:" -> "OK",
      ":v:" -> "V",
      ":clap:" -> "*clap*",
      ":wave:" -> "*wave*",
      ":point_right:" -> "->",
      ":point_left:" -> "<-",
      ":point_up:" -> "^",
      ":point_down:" -> "v",
      
      // Misc
      ":100:" -> "100%",
      ":fire:" -> "*fire*",
      ":star:" -> "*",
      ":star2:" -> "*",
      ":sparkles:" -> "*sparkle*",
      ":tada:" -> "*party*",
      ":skull:" -> "X_X",
      ":skull_and_crossbones:" -> "X_X",
      ":poop:" -> "poop",
      ":hankey:" -> "poop",
      ":ghost:" -> "BOO!",
      ":alien:" -> "ET",
      ":robot:" -> "[O_O]",
      ":jack_o_lantern:" -> "o'-'o",
      
      // Actions/Gestures
      ":pray:" -> "*pray*",
      ":handshake:" -> "*handshake*",
      ":muscle:" -> "*flex*",
      ":raised_hand:" -> "o/",
      ":vulcan_salute:" -> "\\\\//",
      ":shrug:" -> "¯\\_(ツ)_/¯"
    )
    
    emojiMap.foldLeft(message) { case (msg, (emoji, emoticon)) =>
      msg.replace(emoji, emoticon)
    }
  }

}
