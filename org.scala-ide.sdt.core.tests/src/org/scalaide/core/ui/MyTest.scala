package org.scalaide.core.ui

import org.junit.runner.RunWith
import org.junit.Test
import org.junit.internal.runners.JUnit4ClassRunner
import org.junit.Assert
import org.mockito.Mockito._
import org.eclipse.jface.text.ILineTracker
import org.scalaide.ui.internal.editor.MacroLineTracker
import org.eclipse.jface.text.Region

trait A{
  def a(i: Int): String
}

@RunWith(classOf[JUnit4ClassRunner])
class MyTest{
  @Test
  def getLineInformation() {
    val masterTracker = mock(classOf[ILineTracker])

    when(masterTracker.getLineInformation(0)).thenReturn(new Region(0,10))
    when(masterTracker.getLineInformation(3)).thenReturn(new Region(26,15))
    when(masterTracker.getLineInformation(4)).thenReturn(new Region(42,15))

    val t = new MacroLineTracker(masterTracker)

    Assert.assertEquals(new Region(0,10), t.getLineInformation(0))
    Assert.assertEquals(new Region(26,20), t.getLineInformation(3))
    Assert.assertEquals(new Region(47,15), t.getLineInformation(4))
  }

  @Test
  def getLineInformationOfOffset() {
    val masterTracker = mock(classOf[ILineTracker])

    when(masterTracker.getLineInformationOfOffset(5)).thenReturn(new Region(0,10))
    when(masterTracker.getLineInformationOfOffset(28)).thenReturn(new Region(26,15))
    when(masterTracker.getLineInformationOfOffset(35)).thenReturn(new Region(26,15))
    when(masterTracker.getLineInformationOfOffset(45)).thenReturn(new Region(42,15))

    val t = new MacroLineTracker(masterTracker)

    Assert.assertEquals(new Region(0,10), t.getLineInformationOfOffset(5))
    Assert.assertEquals(new Region(26,20), t.getLineInformationOfOffset(30))
    Assert.assertEquals(new Region(26,20), t.getLineInformationOfOffset(40))
    Assert.assertEquals(new Region(47,15), t.getLineInformationOfOffset(50))
  }

  @Test
  def testStore() {

  }
}