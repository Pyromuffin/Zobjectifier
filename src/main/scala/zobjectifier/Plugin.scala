package zobjectifier

import dotty.tools.dotc.ast.Trees.*
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.DenotTransformers.DenotTransformer
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Scopes.newScope
import dotty.tools.dotc.core.Symbols
import dotty.tools.dotc.printing.*
import dotty.tools.dotc.plugins.{PluginPhase, StandardPlugin}
import dotty.tools.dotc.transform.{ElimRepeated, FirstTransform, FullParameterization, ProtectedAccessors}
import dotty.tools.dotc.typer.TyperPhase
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.quoted.Quotes



class Plugin extends StandardPlugin{
  val name: String = "Zobjectifier"
  override val description: String = "objects are now less lazy"

  override def initialize(options: List[String])(using Context) = {
    println("Init Zobjectifier")
    List(new CollectObjects, new AddNames)
  }

}



class AddNames extends PluginPhase {

  import tpd.*

  val phaseName = "AddNames"
  override val runsAfter = Set(TyperPhase.name)


  def isNullLiteral(tree: Tree): Boolean = tree match {
    case Literal(Constant(null)) => true
    case _ => false
  }

  override def transformValDef(tree: tpd.ValDef)(using ctx: Context) : Tree = {

    val thingSymbol = Symbols.getClassIfDefined("Zext.Thing")

    // to catch refined types which for some reason don't count as subclasses
    val valdefType = tree.tpt.tpe match {
      case r : RefinedType => r.parent.typeSymbol
      case  _ => tree.tpt.symbol
    }

    val isThing = valdefType.denot.isSubClass(thingSymbol)
    //println(tree.name.show + " flags " + tree.tpt.symbol.flags.flagsString + " type " + tree.tpt )

    if(isThing && !tree.rhs.isEmpty && !isNullLiteral(tree.rhs)){

      val name = tree.name.show

      val setName = tree.symbol.requiredMethod("SetName")
      //val setNameRef = Ident(setName.namedType)

      val rhs = tree.rhs
      val select = Select(rhs, setName.namedType)
      val setNameTree = Apply(select, List(Literal(Constant(name))))
      val newRhs = setNameTree

     // println(name)
     // println(rhs.show)
     // println(rhs)


      val newTree = cpy.ValDef(tree)(tree.name, tree.tpt, newRhs)
      //println("new rhs")
      //println(newRhs.show)
      //println(newRhs)
      //println("----------")

      newTree
    }
    else tree


  }



}


class CollectObjects extends PluginPhase {

  import tpd.*

  val phaseName = "CollectObjects"
  override val runsAfter = Set(TyperPhase.name)


  override def transformPackageDef(tree: PackageDef)(using ctx : Context): Tree = {


   // println("object count :" + all.size)
   // all.foreach(println)
    /*
    var idx = 0
    for(s <- tree.stats){
      println(idx)
      println(s.show)
      println(s.getClass)
      println(s.symbol.owner)
      println(s.symbol.flags.flagsString)
      idx += 1
    }
*/
    //println("tree symbol " + tree.symbol)

    val objects = ArrayBuffer[Tree]()

    tree.foreachSubTree{
      case vd : ValDef =>{
        if(vd.symbol.flags.isAllOf(Module | StableRealizable | Lazy | Final) && !vd.symbol.flags.isOneOf(Synthetic | Deferred | Private | Accessor)){
          objects.addOne(ref(vd.symbol))
        }
      }
      case _ =>
    }


    val prefix = ctx.compilationUnit.source.name.stripSuffix(".scala")

    val secretClass = newCompleteClassSymbol(tree.symbol.moduleClass, typeName(prefix + "$SecretHolder"), FlagSet(0), defn.ObjectType :: Nil, newScope).entered
    val constructorSymbol = newConstructor(secretClass, Synthetic, Nil, Nil).entered
    val constructorDef = DefDef(constructorSymbol.asTerm)

    val secretMethodSymbol = newSymbol(secretClass, termName("Reveal"), JavaStatic | Method, defn.UnitType).entered
    val body : Tree = Block(objects.toList, Literal(Constant(())))
    val secretMethodDef = DefDef(secretMethodSymbol.asTerm, body)

    val secretClassDef = ClassDef(secretClass, constructorDef, secretMethodDef :: Nil)
    val newPackageDef = cpy.PackageDef(tree)(pid = tree.pid, stats = tree.stats ::: secretClassDef :: Nil)


    /*
    println("========ZEBRA 1")
    println(secretMethodDef.show)
    println("========ZEBRA 2")
    println("package " + newPackageDef.show)
    println("========ZEBRA 3")
    */
    newPackageDef
  }

  /*

override def transformTypeDef(tree: TypeDef)(using ctx: Context): Tree = {

  if (!tree.symbol.isPackageObject)
    return tree

 // println(tree.symbol)
  //println(tree.show)

  val newsym = newSymbol(tree.symbol, termName("Secret"), FlagSet(0), defn.IntType).entered
  val secret = ValDef(newsym.asTerm, Literal(Constant(100)))

  val template = tree.rhs.asInstanceOf[Template]
  val newBody = template.body.appended(secret)
  val newTemplate = cpy.Template(template)(body = newBody)


  val newTree =  cpy.TypeDef(tree)(rhs = newTemplate)


  newTree
}


  override def transformDefDef(tree: DefDef)(using ctx: Context): Tree = {


    tree.setDefTree
    val sym = tree.symbol

    if tree.name != termName("Touch") then
      return tree

 //   println(tree.show)
  //  println(AllObjects.objects)


    // ignore abstract and synthetic methods
    if tree.rhs.isEmpty || sym.isOneOf(Synthetic | Deferred | Private | Accessor | Inline | ConstructorProxyModule)
    then return tree

    try {
     // println("\n\n\n\n")
     // println("========================== tree ==========================")
     // println(tree.show)

      // val body = {tree.rhs}

     // val newsym = newSymbol(tree.symbol, termName("body"), Local, tree.rhs.tpe).asTerm

     // val body = ValDef(newsym, Block(Nil, tree.rhs))

//      val bodyRef = ref(body.symbol)
      val all = AllObjects.packages.flatMap( kv =>  kv._2).filterNot(t => t.symbol.fullName.toString.contains("scala"))
      //printRes
      // shove it all together in a block
      val rhs1 = tpd.Block(all.toList, Literal(Constant(null)) )

      //replace RHS with new
      val newDefDef = cpy.DefDef(tree)(rhs = tree)
      println("====================== transformed ======================")
      println(newDefDef.show)
      newDefDef
    } catch {
      case e =>
   //     println("====================== error ===========================")
    //    println(e)
   //     println(e.printStackTrace)
        tree

    }
  }
  */
}







