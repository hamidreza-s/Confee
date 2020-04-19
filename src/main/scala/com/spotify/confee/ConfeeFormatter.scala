package com.spotify.confee

object ConfeeFormatter {
  def apply(irc: ConfeeIRC, target: ConfeeCompiler.Target): Either[ConfeeError, String] =
    Right("")
}
