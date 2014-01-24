/**
 *  Copyright (c) 2014 by JP Moresmau
 * This code is made available under the terms of the Eclipse Public License,
 * version 1.0 (EPL). See http://www.eclipse.org/legal/epl-v10.html
 */
package net.sf.eclipsefp.haskell.debug.core;


import static org.junit.Assert.assertEquals;
import net.sf.eclipsefp.haskell.debug.core.internal.util.CoreTexts;
import org.junit.Test;


/**
 *
 * @author JP Moresmau
 *
 */
public class DurationTest {

  @Test
  public void testFormatDuration(){
    assertEquals("999 ms",CoreTexts.formatDuration(999L));
    assertEquals("1 second, 999 ms",CoreTexts.formatDuration(1999L));
    assertEquals("2 seconds, 999 ms",CoreTexts.formatDuration(2999L));
    assertEquals("1 minute, 2 seconds, 999 ms",CoreTexts.formatDuration(60000L+2999L));
    assertEquals("2 minutes, 2 seconds, 999 ms",CoreTexts.formatDuration(2*60000L+2999L));
    assertEquals("1 hour, 2 minutes, 2 seconds, 999 ms",CoreTexts.formatDuration(3600*1000+2*60000L+2999L));
    assertEquals("2 hours, 2 minutes, 2 seconds, 999 ms",CoreTexts.formatDuration(3600*2000+2*60000L+2999L));

  }
}
