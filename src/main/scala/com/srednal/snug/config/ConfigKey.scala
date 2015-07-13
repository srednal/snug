package com.srednal.snug.config

case class ConfigKey[X: ConfigConversion](val path: String)
