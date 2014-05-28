package scala.tools.eclipse

import org.eclipse.jface.text.IDocument
import org.eclipse.ui.IEditorInput
import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitEditor
import scala.tools.eclipse.semantichighlighting.implicits.ImplicitHighlightingPresenter
import org.eclipse.jdt.internal.ui.JavaPlugin
import scala.tools.eclipse.javaelements.ScalaCompilationUnit
import org.eclipse.jface.text.Position
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.resources.IResource
import org.eclipse.core.resources.IMarker
import org.eclipse.ui.part.FileEditorInput
import org.eclipse.jface.text.source.IAnnotationModel
import collection.JavaConversions._
import org.eclipse.jface.text.source.Annotation
import org.eclipse.ui.texteditor.MarkerAnnotation

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
      //TODO: out of sync(press F5) doesn't work
  private var macroEpansions: List[Position] = Nil
  private var iEditorOpt: Option[IEditorInput] = None
  private def documentOpt: Option[IDocument] = iEditorOpt.map(getDocumentProvider.getDocument(_))
  private def annotationModelOpt: Option[IAnnotationModel] = iEditorOpt.map(getDocumentProvider.getAnnotationModel(_))

  //private def document: Option[IDocument] = Option(getDocumentProvider.getDocument(iEditorOpt)) Why this compiles?

  override def performSave(overwrite: Boolean, progressMonitor: IProgressMonitor) {
    removeMacroExpansions
    super.performSave(overwrite, progressMonitor)
    expandMacros(iEditorOpt)
    myLog.log("performSave")
  }

  override def doSetInput(iEditorInput: IEditorInput) {
    iEditorOpt = Option(iEditorInput)
    super.doSetInput(iEditorInput)
    expandMacros(iEditorOpt)
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
          doc <- documentOpt
          position <- macroExpandeePositions
        } {
          val lineNumForMacroExpansion = doc.getLineOfOffset(position.getOffset) + 1
          val offsetForMacroExpansion = doc.getLineOffset(lineNumForMacroExpansion)
          val macroExpansion = SuperCompiler.showCode
          doc.replace(offsetForMacroExpansion, 0, macroExpansion)

          //Mark macro expansion lines
          val marker = editorInput.asInstanceOf[FileEditorInput].getFile.createMarker("scala.tools.eclipse.macroMarkerId")
          val end = offsetForMacroExpansion + macroExpansion.length //TODO: remove
          marker.setAttribute(IMarker.CHAR_START, offsetForMacroExpansion)
          marker.setAttribute(IMarker.CHAR_END, offsetForMacroExpansion + macroExpansion.length)
        }
      }
    }
  }

  private def removeMacroExpansions {
    val annotationsOpt = annotationModelOpt.map(_.getAnnotationIterator)
    for {
      doc <- documentOpt
      annotationModel <- annotationModelOpt
      annotations <- annotationsOpt
      annotationNoType <- annotations
    } {
      val annotation = annotationNoType.asInstanceOf[Annotation]
      val tpe = annotation.getType
      if (annotation.getType == "scala.tools.eclipse.macroMarkerId") {
        val pos = annotationModel.getPosition(annotation)
        
        val marker = annotation.asInstanceOf[MarkerAnnotation].getMarker
        marker.delete
        
        doc.replace(pos.offset, pos.length, "")
      }
    }
  }
}