package scala.tools.eclipse.quickfix.abstractimpl

import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal
import scala.tools.eclipse.javaelements.ScalaSourceFile
import scala.collection.immutable
import scala.tools.eclipse.quickfix.explicit.ExpandText

object ImplAbstractMember {
  def suggestsFor(ssf: ScalaSourceFile, offset: Int): immutable.Seq[IJavaCompletionProposal] = {
    implAbstractMember(ssf, offset)
  }

  def implAbstractMember(ssf: ScalaSourceFile, offset: Int): List[IJavaCompletionProposal] = {
    ssf.withSourceFile { (sourceFile, compiler) =>
      import compiler.{Tree, ClassDef, ModuleDef, EmptyTree, TypeTree, Type, Symbol}

      def implAbstractProposals(tree: Tree): List[IJavaCompletionProposal] = {
//        //check tree
//        val list = compiler.askOption{ () =>
//          val tp = tree.symbol.tpe
//          tp.members filter (m => (m.isMethod || m.isValue) && m.isIncompleteIn(tree.symbol) && m.isDeferred && !m.isSetter)
//
//        } map {
//          members =>
//            val res = members map { sym =>
//              new ExpandText(150, sym.toString(), "generic type", offset)
//            }
//            res
//        }
//        list getOrElse Nil toList
        null
      }

      val enclosing = compiler.enclosingClass(sourceFile, offset)
      if (enclosing != EmptyTree) {
        compiler.withResponse[Tree] { response =>
          compiler.askTypeAt(enclosing.pos, response)
        }.get.left.toOption flatMap {
          case cd @ ClassDef(mods, name, tparams, impl) if cd.symbol.isAbstractClass =>
            Option(implAbstractProposals(cd))
          case md @ ModuleDef(mods, name, impl) if md.symbol.isAbstractClass => None
          case _ => None
        } getOrElse(Nil)
      } else Nil
    } getOrElse(Nil)
  }
}