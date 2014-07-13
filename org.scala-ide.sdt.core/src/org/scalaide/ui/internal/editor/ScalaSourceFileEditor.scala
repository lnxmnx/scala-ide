package org.scalaide.ui.internal.editor

import java.util.ResourceBundle
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.SynchronizedBuffer
import org.eclipse.core.runtime.IProgressMonitor
import org.eclipse.core.runtime.Status
import org.eclipse.core.runtime.jobs.Job
import org.eclipse.jdt.core.dom.CompilationUnit
import org.eclipse.jdt.internal.ui.javaeditor.CompilationUnitEditor
import org.eclipse.jdt.internal.ui.javaeditor.selectionactions.SelectionHistory
import org.eclipse.jdt.internal.ui.javaeditor.selectionactions.StructureSelectHistoryAction
import org.eclipse.jdt.internal.ui.javaeditor.selectionactions.StructureSelectionAction
import org.eclipse.jdt.internal.ui.text.java.IJavaReconcilingListener
import org.eclipse.jdt.ui.PreferenceConstants
import org.eclipse.jdt.ui.actions.IJavaEditorActionDefinitionIds
import org.eclipse.jface.action.Action
import org.eclipse.jface.action.IContributionItem
import org.eclipse.jface.action.MenuManager
import org.eclipse.jface.action.Separator
import org.eclipse.jface.text.AbstractReusableInformationControlCreator
import org.eclipse.jface.text.DefaultInformationControl
import org.eclipse.jface.text.IDocument
import org.eclipse.jface.text.IDocumentExtension4
import org.eclipse.jface.text.ITextOperationTarget
import org.eclipse.jface.text.ITextSelection
import org.eclipse.jface.text.ITextViewerExtension
import org.eclipse.jface.text.Position
import org.eclipse.jface.text.information.InformationPresenter
import org.eclipse.jface.text.source.Annotation
import org.eclipse.jface.text.source.IAnnotationModel
import org.eclipse.jface.util.PropertyChangeEvent
import org.eclipse.jface.viewers.ISelection
import org.eclipse.swt.widgets.Shell
import org.eclipse.ui.ISelectionListener
import org.eclipse.ui.IWorkbenchCommandConstants
import org.eclipse.ui.IWorkbenchPart
import org.eclipse.ui.texteditor.IAbstractTextEditorHelpContextIds
import org.eclipse.ui.texteditor.ITextEditorActionConstants
import org.eclipse.ui.texteditor.IUpdate
import org.eclipse.ui.texteditor.TextOperationAction
import org.scalaide.core.ScalaPlugin
import org.scalaide.core.internal.decorators.markoccurrences.Occurrences
import org.scalaide.core.internal.decorators.markoccurrences.ScalaOccurrencesFinder
import org.scalaide.core.internal.jdt.model.ScalaCompilationUnit
import org.scalaide.refactoring.internal.OrganizeImports
import org.scalaide.refactoring.internal.RefactoringHandler
import org.scalaide.refactoring.internal.RefactoringMenu
import org.scalaide.refactoring.internal.source.GenerateHashcodeAndEquals
import org.scalaide.refactoring.internal.source.IntroduceProductNTrait
import org.scalaide.ui.internal.actions
import org.scalaide.ui.internal.editor.autoedits.SurroundSelectionStrategy
import org.scalaide.ui.internal.editor.decorators.semantichighlighting.TextPresentationEditorHighlighter
import org.scalaide.ui.internal.editor.decorators.semantichighlighting.TextPresentationHighlighter
import org.scalaide.ui.internal.preferences.EditorPreferencePage
import org.scalaide.util.internal.Utils
import org.scalaide.util.internal.eclipse.EclipseUtils
import org.scalaide.util.internal.eclipse.EditorUtils
import org.scalaide.util.internal.eclipse.AnnotationUtils._
import org.scalaide.util.internal.ui.DisplayThread
import org.eclipse.ui.IEditorInput
import org.eclipse.jface.text.source.IVerticalRulerColumn
import org.eclipse.jface.text.source.IChangeRulerColumn
import org.eclipse.swt.widgets.Composite
import org.eclipse.jface.text.source.IVerticalRuler
import org.eclipse.jface.text.source.IOverviewRuler
import org.eclipse.jface.preference.IPreferenceStore
import ScalaSourceFileEditor._
import org.eclipse.jdt.internal.ui.javaeditor.JavaSourceViewer
import org.eclipse.jface.text.projection.ProjectionDocumentManager
import org.eclipse.jface.text.projection.ProjectionDocument
import org.eclipse.jface.text.projection.ProjectionDocumentEvent
import org.eclipse.jface.text.DocumentEvent
import org.eclipse.jface.text.ITextStore
import org.eclipse.jface.text.Region
import org.eclipse.jface.text.ILineTracker
import org.eclipse.jface.text.IRegion
import org.eclipse.core.internal.filebuffers.SynchronizableDocument
import org.eclipse.jface.text.AbstractDocument

trait M {
  class MacroRegion(val offset: Int, val length: Int, val text: String) {
    lazy val collision = text.length - length
  }

  var savePeriod = false
  var m = new MacroRegion(offset = 59, length = 0, text = """"123456789"""")

  def map2InnerOffset(offset: Int) =
    if (offset > m.offset)
      if (offset > m.offset + m.length) offset - m.collision
      else m.offset  //if offset is inside macroExpansion return start of macroExpansion
    else offset

  def map2InnerLength(offset: Int, length: Int) = if (offset < m.offset && m.offset + m.length < offset + length) length - m.collision else length

  def map2Inner(offset: Int, length: Int) = (map2InnerOffset(offset), map2InnerLength(offset, length))

  def map2OuterOffset(regOffset: Int) = if (regOffset > m.offset) regOffset + m.collision else regOffset
  def map2OuterLength(regOffset: Int, regLength: Int) = if (regOffset < m.offset && m.offset + m.length < regOffset + regLength) regLength + m.collision else regLength
  def map2Outer(region: IRegion) = new Region(map2OuterOffset(region.getOffset), map2OuterLength(region.getOffset, region.getLength))

  def addMacro(offset: Int, length: Int, innerText: String) = if (offset < m.offset && m.offset + m.length < offset + length) {
    innerText.take(m.offset - offset) + m.text + innerText.drop(m.offset - offset + m.length)
  } else innerText
}

class MacroTextStore(private val master: ITextStore) extends ITextStore with M {
  override def get(offset: Int) =
    if (savePeriod) master.get(offset)
    else master.get(map2InnerOffset(offset))
  override def get(offset: Int, length: Int) =
    if (savePeriod) master.get(offset, length)
    else {
      val (innerOffset, innerLength) = map2Inner(offset, length)
      val innerText = master.get(innerOffset, innerLength)

      addMacro(innerOffset, innerLength, innerText)
    }

  override def getLength() =
    if (savePeriod) master.getLength()
    else {
      if (master.getLength > 0) master.getLength + m.collision
      else master.getLength
    }
  override def replace(offset: Int, length: Int, text: String) =
    if (savePeriod) master.replace(offset, length, text)
    else {
      val (innerOffset, innerLenth) = map2Inner(offset, length)

      master.replace(innerOffset, innerLenth, text)
    }

  override def set(text: String) = master.set(text)

  def replaceMacro(offset: Int, length: Int, text: String) {
    m = new MacroRegion(offset, length, text)
  }
}

class MacroLineTracker(val master: ILineTracker) extends ILineTracker with M {
  override def computeNumberOfLines(text: String): Int = master.computeNumberOfLines(text)
  override def getLegalLineDelimiters(): Array[String] = master.getLegalLineDelimiters()
  override def getLineDelimiter(line: Int): String = master.getLineDelimiter(line)
  override def replace(offset: Int, length: Int, text: String) =
    if (savePeriod) master.replace(offset, length, text)
    else {
      val (innerOffset, innerLenth) = map2Inner(offset, length)

      master.replace(innerOffset, innerLenth, text)
    }

  override def set(text: String): Unit = master.set(text)

  override def getLineInformation(line: Int): IRegion =
    if (savePeriod) master.getLineInformation(line)
    else {
      val mas = master.getLineInformation(line)

      map2Outer(mas)
    }

  override def getLineInformationOfOffset(offset: Int): org.eclipse.jface.text.IRegion =
    if (savePeriod) master.getLineInformationOfOffset(offset)
    else {
      val t = map2InnerOffset(offset)
      val mas = master.getLineInformationOfOffset(t)

      map2Outer(mas)
    }

  override def getLineLength(line: Int): Int = getLineInformation(line).getLength
  override def getLineOffset(line: Int): Int = getLineInformation(line).getOffset
  override def getLineNumberOfOffset(offset: Int): Int = master.getLineNumberOfOffset(map2InnerOffset(offset))
  override def getNumberOfLines(): Int = master.getNumberOfLines()
  override def getNumberOfLines(offset: Int, length: Int): Int =
    if (savePeriod) master.getNumberOfLines(offset, length)
    else {
      val (innerOffset, innerLength) = map2Inner(offset, length)

      master.getNumberOfLines(innerOffset, innerLength)
    }

  def replaceMacro(offset: Int, length: Int, text: String) {
    m = new MacroRegion(offset, length, text)
  }
}

class ScalaSourceFileEditor extends CompilationUnitEditor with ScalaCompilationUnitEditor with ScalaMacroEditor with ScalaLineNumberMacroEditor { self =>
  override def doSetInput(input: IEditorInput) = {
    super.doSetInput(input: IEditorInput)
    val masterStore = getDocumentStore
    val slaveStore = new MacroTextStore(masterStore)
    setDocumentStore(slaveStore)

    val masterTracker = getDocumentTracker
    val slaveTracker = new MacroLineTracker(masterTracker)
    setDocumentTracker(slaveTracker)
  }

  override protected def createLineNumberRulerColumn(): IVerticalRulerColumn = {
    val verticalRuler = new LineNumberChangeRulerColumnWithMacro(getSharedColors)
    verticalRuler.asInstanceOf[IChangeRulerColumn].setHover(createChangeHover)
    initializeLineNumberRulerColumn(verticalRuler)
    verticalRuler
  }

  override def performSave(overwrite: Boolean, progressMonitor: IProgressMonitor) {
    documentStore.savePeriod = true
    documentTracker.savePeriod = true
    super.performSave(overwrite, progressMonitor)
    documentStore.savePeriod = false
    documentTracker.savePeriod = false
  }

  private var occurrenceAnnotations: Set[Annotation] = Set()
  private var occurrencesFinder: ScalaOccurrencesFinder = _
  private var occurencesFinderInstalled = false

  private val reconcilingListeners: ReconcilingListeners = new ScalaSourceFileEditor.ReconcilingListeners

  private lazy val selectionListener = new ISelectionListener() {
    def selectionChanged(part: IWorkbenchPart, selection: ISelection) {
      selection match {
        case textSel: ITextSelection => requireOccurrencesUpdate(textSel)
        case _ =>
      }
    }
  }
  private lazy val tpePresenter = {
    val infoPresenter = new InformationPresenter(controlCreator)
    infoPresenter.install(getSourceViewer)
    infoPresenter.setInformationProvider(actions.TypeOfExpressionProvider, IDocument.DEFAULT_CONTENT_TYPE)
    infoPresenter
  }

  setPartName("Scala Editor")

  override protected def createActions() {
    super.createActions()

    val cutAction = new TextOperationAction(bundleForConstructedKeys, "Editor.Cut.", this, ITextOperationTarget.CUT) //$NON-NLS-1$
    cutAction.setHelpContextId(IAbstractTextEditorHelpContextIds.CUT_ACTION)
    cutAction.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_CUT)
    setAction(ITextEditorActionConstants.CUT, cutAction)

    val copyAction = new TextOperationAction(bundleForConstructedKeys, "Editor.Copy.", this, ITextOperationTarget.COPY, true) //$NON-NLS-1$
    copyAction.setHelpContextId(IAbstractTextEditorHelpContextIds.COPY_ACTION)
    copyAction.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_COPY)
    setAction(ITextEditorActionConstants.COPY, copyAction)

    val pasteAction = new TextOperationAction(bundleForConstructedKeys, "Editor.Paste.", this, ITextOperationTarget.PASTE) //$NON-NLS-1$
    pasteAction.setHelpContextId(IAbstractTextEditorHelpContextIds.PASTE_ACTION)
    pasteAction.setActionDefinitionId(IWorkbenchCommandConstants.EDIT_PASTE)
    setAction(ITextEditorActionConstants.PASTE, pasteAction)

    val selectionHistory = new SelectionHistory(this)

    val historyAction = new StructureSelectHistoryAction(this, selectionHistory)
    historyAction.setActionDefinitionId(IJavaEditorActionDefinitionIds.SELECT_LAST)
    setAction(StructureSelectionAction.HISTORY, historyAction)
    selectionHistory.setHistoryAction(historyAction)

    // disable Java indent logic, which is otherwise invoked when the tab key is entered
    setAction("IndentOnTab", null)

    val selectEnclosingAction = new actions.ScalaStructureSelectEnclosingAction(this, selectionHistory)
    selectEnclosingAction.setActionDefinitionId(IJavaEditorActionDefinitionIds.SELECT_ENCLOSING)
    setAction(StructureSelectionAction.ENCLOSING, selectEnclosingAction)

    val openAction = new Action {
      private def scalaCompilationUnit: Option[ScalaCompilationUnit] =
        Option(getInteractiveCompilationUnit) map (_.asInstanceOf[ScalaCompilationUnit])

      override def run {
        scalaCompilationUnit foreach { scu =>
          scu.followDeclaration(ScalaSourceFileEditor.this, getSelectionProvider.getSelection.asInstanceOf[ITextSelection])
        }
      }
    }
    openAction.setActionDefinitionId(IJavaEditorActionDefinitionIds.OPEN_EDITOR)
    setAction("OpenEditor", openAction)
  }

  override protected def initializeKeyBindingScopes() {
    setKeyBindingScopes(Array(SCALA_EDITOR_SCOPE))
  }

  override def updateOccurrenceAnnotations(selection: ITextSelection, astRoot: CompilationUnit): Unit = {
    requireOccurrencesUpdate(selection)
  }

  /**
   * Returns the annotation model of the current document provider.
   */
  private def getAnnotationModelOpt: Option[IAnnotationModel] = {
    for {
      documentProvider <- Option(getDocumentProvider)
      annotationModel <- Option(documentProvider.getAnnotationModel(getEditorInput))
    } yield annotationModel
  }

  private def performOccurrencesUpdate(selection: ITextSelection, documentLastModified: Long) {
    val annotations = getAnnotations(selection, documentLastModified)
    for (annotationModel <- getAnnotationModelOpt) annotationModel.withLock {
      annotationModel.replaceAnnotations(occurrenceAnnotations, annotations)
      occurrenceAnnotations = annotations.keySet
    }
  }

  private def getAnnotations(selection: ITextSelection, documentLastModified: Long): Map[Annotation, Position] = {
    val region = EditorUtils.textSelection2region(selection)
    val occurrences = occurrencesFinder.findOccurrences(region, documentLastModified)
    for {
      Occurrences(name, locations) <- occurrences.toList
      location <- locations
      annotation = new Annotation(OCCURRENCE_ANNOTATION, false, "Occurrence of '" + name + "'")
      position = new Position(location.getOffset, location.getLength)
    } yield annotation -> position
  }.toMap

  private def requireOccurrencesUpdate(selection: ITextSelection) {

    if (selection.getLength < 0 || selection.getOffset < 0)
      return

    if (getDocumentProvider == null || !isActiveEditor)
      return

    val lastModified = getSourceViewer.getDocument match {
      case document: IDocumentExtension4 =>
        document.getModificationStamp
      case _ => return
    }

    EclipseUtils.scheduleJob("Updating occurrence annotations", priority = Job.DECORATE) { monitor =>
      val fileName = getInteractiveCompilationUnit.file.name
      Utils.debugTimed("Time elapsed for \"updateOccurrences\" in source " + fileName) {
        performOccurrencesUpdate(selection, lastModified)
      }
      Status.OK_STATUS
    }
  }

  override def doSelectionChanged(selection: ISelection) {
    super.doSelectionChanged(selection)
    val selectionProvider = getSelectionProvider
    if (selectionProvider != null)
      selectionProvider.getSelection match {
        case textSel: ITextSelection => requireOccurrencesUpdate(textSel)
        case _ =>
      }
  }

  override def installOccurrencesFinder(forceUpdate: Boolean) {
    if (!occurencesFinderInstalled) {
      super.installOccurrencesFinder(forceUpdate)
      getEditorSite.getPage.addPostSelectionListener(selectionListener)
      occurencesFinderInstalled = true
    }
  }

  override def uninstallOccurrencesFinder() {
    occurencesFinderInstalled = false
    getEditorSite.getPage.removePostSelectionListener(selectionListener)
    super.uninstallOccurrencesFinder
    removeScalaOccurrenceAnnotations()
  }

  /**
   * Clear the existing Mark Occurrences annotations.
   */
  def removeScalaOccurrenceAnnotations() {
    for (annotationModel <- getAnnotationModelOpt) annotationModel.withLock {
      annotationModel.replaceAnnotations(occurrenceAnnotations, Map())
      occurrenceAnnotations = Set()
    }
  }

  /** Return the `InformationPresenter` used to display the type of the selected expression.*/
  def typeOfExpressionPresenter: InformationPresenter = tpePresenter

  override def editorContextMenuAboutToShow(menu: org.eclipse.jface.action.IMenuManager): Unit = {
    super.editorContextMenuAboutToShow(menu)

    def groupMenuItemsByGroupId(items: Seq[IContributionItem]) = {
      // the different groups (as indicated by separators) and
      // contributions in a menu are originally just a flat list
      val groups = items.foldLeft(Nil: List[(String, List[IContributionItem])]) {

        // start a new group
        case (others, group: Separator) => (group.getId, Nil) :: others

        // append contribution to the current group
        case ((group, others) :: rest, element) => (group, element :: others) :: rest

        // the menu does not start with a group, this shouldn't happen, but if
        // it does we just skip this element, so it will stay in the menu.
        case (others, _) => others
      }
      groups.toMap
    }

    def findJdtSourceMenuManager(items: Seq[IContributionItem]) = {
      items.collect {
        case mm: MenuManager if mm.getId == "org.eclipse.jdt.ui.source.menu" => mm
      }
    }

    findJdtSourceMenuManager(menu.getItems) foreach { mm =>

      val groups = groupMenuItemsByGroupId(mm.getItems)

      // these contributions won't work on Scala files, so we remove them
      val blacklist = List("codeGroup", "importGroup", "generateGroup", "externalizeGroup")
      blacklist.flatMap(groups.get).flatten.foreach(mm.remove)

      def action(h: RefactoringHandler, text: String) = new Action {
        setText(text)
        override def run(): Unit = h.perform()
      }

      // and provide our own organize imports instead
      mm.appendToGroup("importGroup", action(new OrganizeImports, "Organize Imports"))

      // add GenerateHashcodeAndEquals and IntroductProductN source generators
      mm.appendToGroup("generateGroup", action(new GenerateHashcodeAndEquals, "Generate hashCode() and equals()..."))
      mm.appendToGroup("generateGroup", action(new IntroduceProductNTrait, "Introduce ProductN trait..."))
    }

    RefactoringMenu.fillContextMenu(menu, this)
  }

  override def createPartControl(parent: org.eclipse.swt.widgets.Composite) {
    super.createPartControl(parent)
    occurrencesFinder = new ScalaOccurrencesFinder(getInteractiveCompilationUnit)
    RefactoringMenu.fillQuickMenu(this)

    getSourceViewer match {
      case sourceViewer: ITextViewerExtension =>
        sourceViewer.prependVerifyKeyListener(new SurroundSelectionStrategy(getSourceViewer))
      case _ =>
    }
  }

  override def handlePreferenceStoreChanged(event: PropertyChangeEvent) = {
    import org.scalaide.core.internal.formatter.FormatterPreferences._
    import scalariform.formatter.preferences._

    val IndentSpacesKey = IndentSpaces.eclipseKey
    val IndentWithTabsKey = IndentWithTabs.eclipseKey

    event.getProperty match {
      case PreferenceConstants.EDITOR_MARK_OCCURRENCES =>
      // swallow the event. We don't want 'mark occurrences' to be linked to the Java editor preference
      case EditorPreferencePage.P_ENABLE_MARK_OCCURRENCES =>
        (event.getNewValue: Any) match {
          case true =>
            installOccurrencesFinder(true)
          case _ =>
            uninstallOccurrencesFinder()
        }

      case IndentSpacesKey | IndentWithTabsKey =>
        val tabWidth = getSourceViewerConfiguration().getTabWidth(sourceViewer)
        sourceViewer.getTextWidget().setTabs(tabWidth)
        updateIndentPrefixes()

      case _ =>
        if (affectsTextPresentation(event)) {
          // those events will trigger an UI change
          DisplayThread.asyncExec(super.handlePreferenceStoreChanged(event))
        } else {
          super.handlePreferenceStoreChanged(event)
        }
    }
  }

  override def isMarkingOccurrences =
    scalaPrefStore.getBoolean(EditorPreferencePage.P_ENABLE_MARK_OCCURRENCES)

  override def createSemanticHighlighter: TextPresentationHighlighter =
    TextPresentationEditorHighlighter(this, semanticHighlightingPreferences, addReconcilingListener _, removeReconcilingListener _)

  override def forceSemanticHighlightingOnInstallment: Boolean = false // relies on the Java reconciler to refresh the highlights

  def addReconcilingListener(listener: IJavaReconcilingListener): Unit =
    reconcilingListeners.addReconcileListener(listener)

  def removeReconcilingListener(listener: IJavaReconcilingListener): Unit =
    reconcilingListeners.removeReconcileListener(listener)

  override def aboutToBeReconciled(): Unit = {
    super.aboutToBeReconciled()
    reconcilingListeners.aboutToBeReconciled()
  }

  override def reconciled(ast: CompilationUnit, forced: Boolean, progressMonitor: IProgressMonitor): Unit = {
    super.reconciled(ast, forced, progressMonitor)
    reconcilingListeners.reconciled(ast, forced, progressMonitor)
  }
}

object ScalaSourceFileEditor {
  private val EDITOR_BUNDLE_FOR_CONSTRUCTED_KEYS = "org.eclipse.ui.texteditor.ConstructedEditorMessages"
  private val bundleForConstructedKeys = ResourceBundle.getBundle(EDITOR_BUNDLE_FOR_CONSTRUCTED_KEYS)

  private val SCALA_EDITOR_SCOPE = "scala.tools.eclipse.scalaEditorScope"

  private val OCCURRENCE_ANNOTATION = "org.eclipse.jdt.ui.occurrences"

  private object controlCreator extends AbstractReusableInformationControlCreator {
    override def doCreateInformationControl(shell: Shell) =
      new DefaultInformationControl(shell, true)
  }

  /** A thread-safe object for keeping track of Java reconciling listeners.*/
  private class ReconcilingListeners extends IJavaReconcilingListener {
    private val reconcilingListeners = new ArrayBuffer[IJavaReconcilingListener] with SynchronizedBuffer[IJavaReconcilingListener]

    /** Return a snapshot of the currently registered `reconcilingListeners`. This is useful to avoid concurrency hazards when iterating on the `reconcilingListeners`. */
    private def currentReconcilingListeners: List[IJavaReconcilingListener] = reconcilingListeners.toList

    override def aboutToBeReconciled(): Unit =
      for (listener <- currentReconcilingListeners) listener.aboutToBeReconciled()

    override def reconciled(ast: CompilationUnit, forced: Boolean, progressMonitor: IProgressMonitor): Unit =
      for (listener <- currentReconcilingListeners) listener.reconciled(ast, forced, progressMonitor)

    def addReconcileListener(listener: IJavaReconcilingListener): Unit = reconcilingListeners += listener

    def removeReconcileListener(listener: IJavaReconcilingListener): Unit = reconcilingListeners -= listener
  }
}
