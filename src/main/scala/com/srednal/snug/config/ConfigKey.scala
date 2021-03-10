package com.srednal.snug.config

import scala.annotation.unused

case class ConfigKey[X](path: String)(@unused implicit val conversion: ConfigConversion[X])
