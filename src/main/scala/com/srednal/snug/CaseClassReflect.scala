package com.srednal.snug

import scala.reflect.runtime.universe._

object CaseClassReflect {

  def isCaseClass(that: Any) = _isCaseClass(typeFor(that.getClass))
  def isCaseClass[C: TypeTag] = _isCaseClass(typeOf[C])
  def _isCaseClass(t: Type) = t.typeSymbol.asClass.isCaseClass

  def caseParamTypes(that: Any): Seq[(String, Type)] = _caseParamTypes(typeFor(that.getClass))

  def caseParamTypes[C: TypeTag]: Seq[(String, Type)] = _caseParamTypes(typeOf[C])


  def typeFor(c: Class[_]) = runtimeMirror(c.getClassLoader).staticClass(c.getCanonicalName).selfType

  def _caseParamTypes(t: Type): Seq[(String, Type)] = t.decls.sorted collect {
    case s: MethodSymbolApi if s.isCaseAccessor => s.name.decodedName.toString -> s.accessed.typeSignatureIn(t)
  }
}
