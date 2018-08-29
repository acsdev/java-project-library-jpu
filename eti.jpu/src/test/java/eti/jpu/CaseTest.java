package eti.jpu;

import java.util.Optional;

import org.junit.Assert;

import org.junit.Test;
/**
 * Provides an new way to execute conditional rotines
 */
public class CaseTest {
    
    @Test
    public void testWithRunnable() {
        
        Runnable rPrintTrue  = () -> System.out.println( "TRUE" );
        Runnable rPrintFalse = () -> System.out.println( "FALSE" );

        Case.of( "A".equalsIgnoreCase("a") ).trueRun( rPrintTrue ).falseRun( rPrintFalse );
        
        Case.of( ( s ) -> "A".equalsIgnoreCase(s), "A" ).trueRun( rPrintTrue ).falseRun( rPrintFalse );
        
        Case.of( Optional.of("A") ).trueRun( rPrintTrue ).falseRun( rPrintFalse );
        
        Case.of( "A".equalsIgnoreCase("b") ).trueRun( rPrintTrue ).falseRun( rPrintFalse );
        
        Case.of( ( s ) -> "A".equalsIgnoreCase(s), "b" ).trueRun( rPrintTrue ).falseRun( rPrintFalse );

        Case.of( Optional.empty() ).trueRun( rPrintTrue ).falseRun( rPrintFalse );
    }
    
}