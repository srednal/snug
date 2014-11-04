package com.srednal.snug

import scala.reflect.runtime.universe._

object CaseClassReflect {

  def isCaseClass(that: Any) = typeIsCaseClass(typeFor(that.getClass))
  def isCaseClass[C: TypeTag] = typeIsCaseClass(typeOf[C])
  def typeIsCaseClass(t: Type) = t.typeSymbol.asClass.isCaseClass

  def caseParamTypes(that: Any): Seq[(String, Type)] = caseParamTypesForType(typeFor(that.getClass))

  def caseParamTypes[C: TypeTag]: Seq[(String, Type)] = caseParamTypesForType(typeOf[C])


  def typeFor(c: Class[_]) = runtimeMirror(c.getClassLoader).staticClass(c.getCanonicalName).selfType

  def caseParamTypesForType(t: Type): Seq[(String, Type)] = t.decls.sorted collect {
    case s: MethodSymbolApi if s.isCaseAccessor => s.name.decodedName.toString -> s.accessed.typeSignatureIn(t)
  }

  def createForType(t: Type)(args: Seq[_]) = {
    val ctr = t.decl(termNames.CONSTRUCTOR).alternatives.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.head
    runtimeMirror(classLoader).reflectClass(t.typeSymbol.asClass).reflectConstructor(ctr)(args: _*)
  }

  def create[C: TypeTag](args: Seq[_]) = createForType(typeOf[C])(args)

  private def classLoader = Thread.currentThread().getContextClassLoader match {
    case cl: ClassLoader => cl
    case _ => getClass.getClassLoader
  }
}
