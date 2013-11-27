package scala.tools.eclipse.quickfix.abstractimpl

import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal
import scala.tools.eclipse.javaelements.ScalaSourceFile
import scala.collection.immutable
import scala.tools.eclipse.quickfix.explicit.ExpandText
import scala.tools.refactoring.implementations.AddToClosest
import org.eclipse.jface.text.Position
import org.eclipse.jdt.core.ICompilationUnit
import scala.tools.eclipse.quickfix.createmethod.{ ParameterList, ReturnType }
import scala.tools.refactoring.implementations.AddMethod
import scala.tools.refactoring.implementations.AddMethodTarget
import scala.tools.eclipse.util.parsing.ScalariformParser
import org.eclipse.jface.text.contentassist.IContextInformation
import org.eclipse.swt.graphics.Image
import org.eclipse.swt.graphics.Point
import org.eclipse.text.edits.ReplaceEdit
import scala.sprinter.printers.TypePrinters
import org.eclipse.jface.text.IDocument
import org.eclipse.jdt.internal.ui.JavaPluginImages
import scala.tools.eclipse.javaelements.ScalaCompilationUnit
import scala.tools.eclipse.refactoring.EditorHelpers
import scala.reflect.internal.util.RangePosition
import scala.reflect.internal.util.SourceFile

object ImplAbstractMember {
  def suggestsFor(ssf: ScalaSourceFile, offset: Int): Array[IJavaCompletionProposal] = {
    implAbstractMember(ssf, offset).toArray //, compUnit).toArray
  }

  def implAbstractMember(ssf: ScalaSourceFile, offset: Int): List[IJavaCompletionProposal] = { //, compUnit: ICompilationUnit): List[IJavaCompletionProposal] = {
    ssf.withSourceFile { (sourceFile, compiler) =>
      import compiler.{ Tree, ClassDef, ModuleDef, EmptyTree, TypeTree, Type, Symbol, Name }

      type NamedTree = Tree { val name: Name; val impl: Tree }

      class AbstractMemberProposal(abstrMember: Symbol, cl: NamedTree, target: AddMethodTarget)  extends IJavaCompletionProposal { //, compilationUnit: ICompilationUnit, pos: Position) extends IJavaCompletionProposal {

        private val UnaryMethodNames = "+-!~".map("unary_" + _)

        //compilationUnit.asInstanceOf[ScalaSourceFile]
        private val sourceAst = ScalariformParser.safeParse(ssf.getSource()).map(_._1)

//        private def typeAtRange(start: Int, end: Int): String = {
//          ssf.withSourceFile((srcFile, compiler) => {
//            compiler.askOption(() => {
//              val length = end - start
//              val context = compiler.doLocateContext(new RangePosition(srcFile, start, start, start + length - 1))
//
//              val tree = compiler.locateTree(new RangePosition(srcFile, start, start, start + length - 1))
//              val typer = compiler.analyzer.newTyper(context)
//              val typedTree = typer.typed(tree)
//              val tpe = typedTree.tpe.resultType.underlying
//              if (tpe.isError) None
//              else if (tpe.toString == "Null") Some("AnyRef") //there must be a better condition
//              else {
//                val sprinterType = TypePrinters.showType(compiler, tpe, context)
//                Some(sprinterType) //do we want tpe.isError? tpe.isErroneous?
//              }
//            }).flatten.getOrElse("Any")
//          }) getOrElse ("Any")
//        }

        import scala.tools.eclipse.quickfix.createmethod.MissingMemberInfo

        private def initValOrDef: (ParameterList, ReturnType) = {
//          compiler.askOption(() => {
            def refactContextPos =
              if (!Option(cl.impl).isEmpty && !cl.impl.isEmpty && !cl.impl.children.isEmpty) {
                cl.impl.children.last match {
                  case dd : compiler.DefDef =>
                    val ndd = dd.name.toString()
                  case o =>
                    val otstr = o.toString
                }
                cl.impl.children.last.pos
              } else cl.pos //TODO fix and test it
            val before = 1
//            val refactPos = refactContextPos
//            val refactPosPoint = refactPos.point
//            val refactRangePos = compiler.rangePos(sourceFile, refactPosPoint, refactPosPoint, refactPosPoint)
//            val refactRangePosEnd = refactRangePos.end
//            val clPos = cl.pos

            val scalaFile = cl.pos.source
            val clPos2isRange = cl.pos.isRange
            val clPos2 = cl.pos
            val rangePos2 = compiler.rangePos(scalaFile, cl.pos.end, cl.pos.end, cl.pos.end)

            val rangePos = compiler.rangePos(sourceFile, cl.pos.end-1, cl.pos.end-1, cl.pos.end-1)
            val sprinterContext = compiler.locateContext(clPos2)
            val after = 2
            def processType(tp: Type) =
              (if (tp.isError) None
              else if (tp.toString == "Null") Some("AnyRef") //there must be a better condition
              else {
                val sprinterType = TypePrinters.showType(compiler, tp, sprinterContext.getOrElse(null))
                Some(sprinterType) //do we want tpe.isError? tpe.isErroneous?
              }) getOrElse ("Any")

//            val method = abstrMember.asMethod
//            val paramss: ParameterList = method.paramss map {
//              _.map { param =>
//                (param.name.decode, processType(param.tpe))
//              }
//            }

            val paramss: ParameterList = List(List(("x", "Float")))

//            val tparams: List[String] = method.typeParams map (_.name.decode)
            val retType: ReturnType = Option("Double"); //Option(processType(method.returnType.asSeenFrom(cl.symbol.tpe, method.owner)))
            (paramss, retType)
//          }).getOrElse((List(Nil), Option("")))
        }

        private val (parameters: ParameterList, returnType: ReturnType) = initValOrDef //

        //  def isApplicable = ???

        override def apply(document: IDocument): Unit = {
          for {
            //we must open the editor before doing the refactoring on the compilation unit:
            theDocument <- EditorHelpers.findOrOpen(ssf.workspaceFile)
          } {
            val scu = ssf.getCompilationUnit.asInstanceOf[ScalaCompilationUnit]
            val changes = scu.withSourceFile { (srcFile, compiler) =>
              val refactoring = new AddMethod { val global = compiler }
              refactoring.addMethod(sourceFile.file, cl.name.decode, abstrMember.nameString, parameters, returnType, target) //if we're here, className should be defined because of the check in isApplicable
            } getOrElse Nil

            for (change <- changes) {
              val edit = new ReplaceEdit(change.from, change.to - change.from, change.text)
              edit.apply(theDocument)
            }

            //TODO: we should allow them to change parameter names and types by tabbing
            for (change <- changes.headOption) {
              val offset = change.from + change.text.lastIndexOf("???")
              EditorHelpers.enterLinkedModeUi(List((offset, "???".length)), selectFirst = true)
            }
          }
        }

        override def getDisplayString(): String = {
          val prettyParameterList = (for (parameterList <- parameters) yield {
            parameterList.map(_._2).mkString(", ")
          }).mkString("(", ")(", ")")

          val returnTypeStr = returnType.map(": " + _).getOrElse("")

          val base = s"Implement method '${abstrMember.nameString}$prettyParameterList$returnTypeStr'"
          base
        }

        override def getRelevance = 90
        override def getSelection(document: IDocument): Point = null
        override def getAdditionalProposalInfo(): String = null
        override def getImage(): Image = JavaPluginImages.DESC_MISC_PUBLIC.createImage()
        override def getContextInformation: IContextInformation = null
      }

      def implAbstractProposals(tree: NamedTree): List[IJavaCompletionProposal] = {
        //check tree
        val list = compiler.askOption { () =>
          val tp = tree.symbol.tpe
          //TODO check not to display deferred methods local to this class (check owner)
          val mems = tp.members filter {m =>
            val misMethod = m.isMethod
            val misValue = m.isValue
            val misIncIn = m.isIncompleteIn(tree.symbol)
            val misDeferred = m.isDeferred
            val mnisSetter = !m.isSetter
            val mownneqsym = (m.owner != tree.symbol)
            val own = m.owner
            val trsym = tree.symbol
            (m.isMethod || m.isValue) && m.isIncompleteIn(tree.symbol) && m.isDeferred && !m.isSetter} // && (m.owner != tree.symbol))
          val res = (mems map {
            sym =>
              new AbstractMemberProposal(sym, tree, AddToClosest(offset))
          }).toList

          res
//        }

//        val fin = list map {
//          members =>
//            val res = members map { sym =>
//              //creation of completion goes here
//              new AbstractMemberProposal(sym, tree, AddToClosest(offset)) //, compUnit, new Position(offset))
//            }
//            res
//        }
//        fin getOrElse Nil toList
        } getOrElse Nil
        list
      }

      def enclosingClassOrModule(src: SourceFile, offset: Int) =
        compiler.locateIn(compiler.parseTree(src), compiler.rangePos(src, offset, offset, offset),
            t => (t.isInstanceOf[ClassDef] || t.isInstanceOf[ModuleDef]))

      val enclosing = enclosingClassOrModule(sourceFile, offset)
      if (enclosing != EmptyTree) {

        compiler.withResponse[Tree] { response =>
          compiler.askTypeAt(enclosing.pos, response)
        }.get.left.toOption flatMap {
          case cd @ ClassDef(mods, name, tparams, impl) => // if cd.symbol.isAbstractClass =>
            Option(implAbstractProposals(cd))
          case md @ ModuleDef(mods, name, impl) => //if md.symbol.isAbstractClass => None
            Option(implAbstractProposals(md))
          case _ => None
        } getOrElse (Nil)
      } else Nil
    } getOrElse (Nil)
  }
}