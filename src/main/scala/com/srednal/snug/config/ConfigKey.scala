package com.srednal.snug.config

class ConfigKey[X: ConfigConversion](val path: String)

object ConfigKey {
  def apply[X: ConfigConversion](path: String): ConfigKey[X] = new ConfigKey(path)
}
