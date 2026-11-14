package wowchat.game

import net.dv8tion.jda.api.components.container.Container

trait GameCommandHandler {

  def sendMessageToWow(tp: Byte, message: String, target: Option[String])
  def sendNotification(message: String)

  def handleWho(arguments: Option[String]): Option[String]
  def buildGuildiesOnlineMessages: Seq[Container]
  def handleGmotd(): Option[String]
}
