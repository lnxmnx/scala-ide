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
import org.eclipse.jdt.ui.text.folding.DefaultJavaFoldingStructureProvider
import org.eclipse.jdt.ui.text.folding.IJavaFoldingStructureProvider
import org.eclipse.ui.texteditor.AbstractDecoratedTextEditor

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

//https://issues.scala-lang.org/browse/SI-7946
//class MacroFoldingStructureProvider extends DefaultJavaFoldingStructureProvider {
//  import org.eclipse.jdt.core.IJavaElement
//  import org.eclipse.jdt.ui.text.folding.DefaultJavaFoldingStructureProvider.FoldingStructureComputationContext
//  myLog.log("computeFoldingStructure")
//    override protected def computeFoldingStructure(element: IJavaElement, ctx: FoldingStructureComputationContext) {
//      myLog.log("computeFoldingStructure")
//      super.computeFoldingStructure(element, ctx)
//    }
//}

trait ScalaMacroLineNumbers { self: ScalaMacroEditor => 
  import org.eclipse.jface.text.source.LineNumberChangeRulerColumn
  import org.eclipse.jface.text.source.ISharedTextColors

  class MyRange(val start: Int, val end: Int) {}

  var macroExpansionLines: List[MyRange] = Nil
  
  class LineNumberChangeRulerColumnWithMacro(sharedColors: ISharedTextColors)
    extends LineNumberChangeRulerColumn(sharedColors) {
    override def createDisplayString(line: Int): String = {
      getMacroExpansionLines      
      macroExpansionLines.flatMap(range =>
        if (range.start <= line && line < range.end) {
          Some(range.start - 1)
        } else None).reduceOption(_ min _).getOrElse(line).toString
//        (line - macroExpansionLines.flatMap(range =>
//        	Math.min(range.end,line) - range.start + 1
//        ).sum).toString
    }
  }
  
  def getCurrentMacroPositions = {
    val annotationsOpt = annotationModelOpt.map(_.getAnnotationIterator)

    var t: List[Position] = Nil
    for {
      doc <- documentOpt
      annotationModel <- annotationModelOpt
      annotations <- annotationsOpt
      annotationNoType <- annotations
    } {
      val annotation = annotationNoType.asInstanceOf[Annotation]
      if (annotation.getType == "scala.tools.eclipse.macroMarkerId") {
        t = annotationModel.getPosition(annotation) :: t
      }
    }
    t
  }
  
  def getMacroExpansionLines(){
    val currentMacroPositions = getCurrentMacroPositions
    macroExpansionLines = 
      for{
        currentMacroPosition <- currentMacroPositions
        doc <- documentOpt      	
      	} yield new MyRange(
        doc.getLineOfOffset(currentMacroPosition.offset), 
        doc.getLineOfOffset(currentMacroPosition.offset + currentMacroPosition.length)
    )
  }
}

trait ScalaMacroEditor extends CompilationUnitEditor with ScalaMacroLineNumbers {
  //TODO: out of sync(press F5) doesn't work
  //TODO: maybe add listener to macroApplication's change? If changed replace macro expansion
  //TODO: macroexpansions do not change untill save command triggered
  protected var iEditorOpt: Option[IEditorInput] = None
  protected def documentOpt: Option[IDocument] = iEditorOpt.map(getDocumentProvider.getDocument(_))
  protected def annotationModelOpt: Option[IAnnotationModel] = iEditorOpt.map(getDocumentProvider.getAnnotationModel(_))

  //private def document: Option[IDocument] = Option(getDocumentProvider.getDocument(iEditorOpt)) Why this compiles?

  override def performSave(overwrite: Boolean, progressMonitor: IProgressMonitor) {
    removeMacroExpansions
    super.performSave(overwrite, progressMonitor)
    expandMacros(iEditorOpt)
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
    val annotations = annotationModelOpt.map(_.getAnnotationIterator)
    for {
      doc <- documentOpt
      annotationModel <- annotationModelOpt
      annotations <- annotations
      annotationNoType <- annotations
    } {
      val annotation = annotationNoType.asInstanceOf[Annotation]
      if (annotation.getType == "scala.tools.eclipse.macroMarkerId") {
        val pos = annotationModel.getPosition(annotation)

        val marker = annotation.asInstanceOf[MarkerAnnotation].getMarker
        marker.delete

        doc.replace(pos.offset, pos.length, "")
      }
    }
  }
}