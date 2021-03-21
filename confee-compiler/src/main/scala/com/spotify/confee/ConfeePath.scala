package com.spotify.confee

trait ConfeePath {

  var path: Option[String] = None

  def setPath(newPath: String): this.type = {
    if (path.isEmpty) path = Some(newPath)
    this
  }
}
