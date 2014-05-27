package scala.tools.eclipse

import org.eclipse.jface.text.IDocument
import org.eclipse.ui.IEditorInput
import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitEditor
import scala.tools.eclipse.semantichighlighting.implicits.ImplicitHighlightingPresenter
import org.eclipse.jdt.internal.ui.JavaPlugin
import scala.tools.eclipse.javaelements.ScalaCompilationUnit
import org.eclipse.jface.text.Position
import org.eclipse.core.runtime.IProgressMonitor

object myLog { //TODO: remove
  import java.io.PrintWriter
  import java.io.File
  val writer = new PrintWriter(new File("/home/nikiforo/logger.log"))
  def log(s: String) {
    writer.write(s + "\n")
    writer.flush
  }
  def log(any: Any) {
    log(any.toString + "\n")
  }
}

object SuperCompiler { //TODO: remove
  val showCode = """val t="Hello world" 
					 |t + "!"
					 |println(t)
					 |""".stripMargin
}

trait ScalaMacroEditor extends CompilationUnitEditor {
  private var macroEpansions: List[Position] = Nil
  private var iEditor: Option[IEditorInput] = None
  private def document: Option[IDocument] = iEditor.map(getDocumentProvider.getDocument(_))
  
//private def document: Option[IDocument] = Option(getDocumentProvider.getDocument(iEditor)) Why this compiles?  
//  private var document: Option[IDocument] = None

  override def performSave(overwrite: Boolean, progressMonitor: IProgressMonitor) {
    removeMacroExpansions
    super.performSave(overwrite, progressMonitor)
    expandMacros(iEditor)
    myLog.log("performSave")
  }

  override def doSetInput(iEditorInput: IEditorInput) {
    iEditor = Option(iEditorInput)
    super.doSetInput(iEditorInput)
    expandMacros(iEditor)
  }

  private def expandMacros(iEditor: Option[IEditorInput]) {
    import scala.tools.eclipse.util.Utils._
    for {
      editorInput <- iEditor
      compilationUnit <- Option(JavaPlugin.getDefault.getWorkingCopyManager.getWorkingCopy(editorInput))
      scu <- compilationUnit.asInstanceOfOpt[ScalaCompilationUnit]
    } {
      scu.doWithSourceFile { (sourceFile, compiler) =>
        import compiler.Traverser
        import compiler.Tree

        var macroExpandeePositions: List[Position] = Nil

        def getMacroExpansionPos(v: Tree) = {
          val Some(macroExpansionAttachment) = v.attachments.get[compiler.MacroExpansionAttachment]
          val originalTree = macroExpansionAttachment.original

          val posStart = originalTree.pos.start
          val posLength = originalTree.pos.end - posStart
          val pos = new Position(posStart, posLength)
          pos
        }

        new Traverser {
          override def traverse(t: Tree): Unit = {
            t match {
              case v if v.attachments.get[compiler.MacroExpansionAttachment].isDefined =>
                val pos = getMacroExpansionPos(v)
                macroExpandeePositions = pos :: macroExpandeePositions
              case _ =>
            }
            super.traverse(t)
          }
        }.traverse(compiler.loadedType(sourceFile).fold(identity, _ => compiler.EmptyTree))

        for {
          doc <- document
          position <- macroExpandeePositions
        } {
          val lineNumForMacroExpansion = doc.getLineOfOffset(position.getOffset) + 1
          val offsetForMacroExpansion = doc.getLineOffset(lineNumForMacroExpansion)
          val macroExpansion = SuperCompiler.showCode
          doc.replace(offsetForMacroExpansion, 0, macroExpansion)

          //somehow mark macro expansion lines
          macroEpansions = new Position(offsetForMacroExpansion, macroExpansion.length) :: macroEpansions
        }

        //TODO: Add markers
      }
    }
  }
  
  private def removeMacroExpansions {
    for {
      doc <- document
      expansion <- macroEpansions
    } {
      doc.replace(expansion.offset, expansion.length, "")
    }
  }
}