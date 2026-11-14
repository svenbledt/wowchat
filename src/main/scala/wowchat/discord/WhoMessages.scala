package wowchat.discord

import java.awt.Color

import net.dv8tion.jda.api.components.container.{Container, ContainerChildComponent}
import net.dv8tion.jda.api.components.separator.Separator
import net.dv8tion.jda.api.components.textdisplay.TextDisplay
import wowchat.commands.WhoResponse
import wowchat.game.{GameResources, GuildMember}

import scala.collection.JavaConverters._
import scala.collection.mutable

// Discord Components V2 messages for the ?who / ?online command
object WhoMessages {

  private val ONLINE_COLOR = new Color(0x2ECC71)
  private val OFFLINE_COLOR = new Color(0x95A5A6)
  private val NOT_FOUND_COLOR = new Color(0xE74C3C)
  private val GUILD_COLOR = new Color(0x3498DB)

  // Components V2 messages allow 4000 characters of text in total; leave headroom for header/footer
  private val MAX_LIST_TEXT = 3500

  def onlinePlayer(r: WhoResponse): Container = {
    card(
      ONLINE_COLOR,
      header(r.playerName, Some(r.guildName).filter(_.nonEmpty)),
      s"**Level** ${r.lvl}  ·  **Class** ${r.cls}  ·  **Race** ${r.race}\n**Zone** ${r.zone}",
      "🟢 Online"
    )
  }

  def offlineGuildMember(member: GuildMember, cls: String, guildName: Option[String]): Container = {
    card(
      OFFLINE_COLOR,
      header(member.name, guildName),
      s"**Level** ${member.level}  ·  **Class** ${cls}\n**Last seen** ${formatLastSeen(member.lastLogoff)} ago in ${zoneName(member.zoneId)}",
      "⚫ Offline"
    )
  }

  def notFound(playerName: String): Container = {
    Container.of(TextDisplay.of(s"No player named **${escape(playerName)}** is currently playing."))
      .withAccentColor(NOT_FOUND_COLOR)
  }

  // One container per message; split into several when the list is too long for one message
  def guildOnline(guildName: Option[String], members: Seq[(GuildMember, String)]): Seq[Container] = {
    val title = guildName.fold("Guild members online")(name => s"<${escape(name)}> online")
    val footer = s"${members.size} guildie${if (members.size != 1) "s" else ""} online"

    if (members.isEmpty) {
      return Seq(card(GUILD_COLOR, s"## $title", "Currently no guildies online.", footer))
    }

    val lines = members.map {
      case (member, cls) => s"**${escape(member.name)}**  ·  ${member.level} $cls  ·  ${zoneName(member.zoneId)}"
    }

    val chunks = mutable.ArrayBuffer(new StringBuilder)
    lines.foreach(line => {
      if (chunks.last.nonEmpty && chunks.last.length + line.length + 1 > MAX_LIST_TEXT) {
        chunks += new StringBuilder
      }
      if (chunks.last.nonEmpty) {
        chunks.last.append('\n')
      }
      chunks.last.append(line)
    })

    chunks.zipWithIndex.map {
      case (chunk, index) =>
        val children = mutable.ArrayBuffer.empty[ContainerChildComponent]
        if (index == 0) {
          children += TextDisplay.of(s"## $title")
          children += Separator.createDivider(Separator.Spacing.SMALL)
        }
        children += TextDisplay.of(chunk.toString)
        if (index == chunks.size - 1) {
          children += Separator.createDivider(Separator.Spacing.SMALL)
          children += TextDisplay.of(s"-# $footer")
        }
        Container.of(children.asJava).withAccentColor(GUILD_COLOR)
    }
  }

  private def card(color: Color, headerText: String, body: String, footer: String): Container = {
    Container.of(
      TextDisplay.of(headerText),
      Separator.createDivider(Separator.Spacing.SMALL),
      TextDisplay.of(body),
      TextDisplay.of(s"-# $footer")
    ).withAccentColor(color)
  }

  private def header(name: String, guildName: Option[String]): String = {
    s"## ${escape(name)}" + guildName.fold("")(guild => s"\n<${escape(guild)}>")
  }

  private def zoneName(zoneId: Int): String = {
    GameResources.AREA.getOrElse(zoneId, "Unknown Zone")
  }

  // lastLogoff is reported in days (fractional)
  private def formatLastSeen(lastLogoff: Float): String = {
    val days = lastLogoff.toInt
    val hours = ((lastLogoff * 24) % 24).toInt
    val minutes = ((lastLogoff * 24 * 60) % 60).toInt

    def unit(value: Int, name: String) = s"$value $name${if (value != 1) "s" else ""}"

    Seq(
      if (days > 0) Some(unit(days, "day")) else None,
      if (hours > 0) Some(unit(hours, "hour")) else None,
      Some(unit(minutes, "minute"))
    ).flatten.mkString(", ")
  }

  // Escape Discord markdown in player and guild names
  private def escape(text: String): String = {
    text.replace("*", "\\*").replace("_", "\\_").replace("~", "\\~").replace("`", "\\`")
  }
}
