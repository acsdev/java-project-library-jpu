package eti.jpu;

import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.junit.Assert;

import org.junit.Test;
/**
 * Provides an new way to execute conditional rotines
 */
public class CaseTest {
    
    @Test
    public void testWithRunnable() {
        Runnable rPrintTrue  = () -> Assert.assertTrue(true);
        Runnable rPrintFalse = () -> Assert.assertFalse(false);

        Case.of( "A".equalsIgnoreCase("a") )
            .trueRun( rPrintTrue )
            .falseRun( rPrintFalse );
        
        Case.of( ( s ) -> "A".equalsIgnoreCase(s), "A" )
            .trueRun( rPrintTrue )
            .falseRun( rPrintFalse );
        
        Case.of( Optional.of("A") )
            .trueRun( rPrintTrue )
            .falseRun( rPrintFalse );
        
        Case.of( "A".equalsIgnoreCase("b") )
            .trueRun( rPrintTrue )
            .falseRun( rPrintFalse );
        
        Case.of( ( s ) -> "A".equalsIgnoreCase(s), "b" )
            .trueRun( rPrintTrue )
            .falseRun( rPrintFalse );

        Case.of( Optional.empty() )
            .trueRun( rPrintTrue )
            .falseRun( rPrintFalse );
    }
        
    @Test
    public void testWithSupplier() {

        Supplier<String> sPrintTrue  = () -> "TRUE";
        Supplier<String> sPrintFalse = () -> "FALSE";

        Assert.assertEquals("TRUE",
            Case.of( "A".equalsIgnoreCase("a") )
                .trueGet( sPrintTrue )
                .falseGet( sPrintFalse ).getResult().get() );
        
        Assert.assertEquals("TRUE",
            Case.of( ( s ) -> "A".equalsIgnoreCase(s), "A" )
                .trueGet( sPrintTrue )
                .falseGet( sPrintFalse ).getResult().get() );

        Assert.assertEquals("TRUE", 
            Case.of( Optional.of("A") )
                .trueGet( sPrintTrue )
                .falseGet( sPrintFalse ).getResult().get() );

        Assert.assertEquals("FALSE",
            Case.of( "A".equalsIgnoreCase("b") )
                .trueGet( sPrintTrue )
                .falseGet( sPrintFalse ).getResult().get() );
        
        Assert.assertEquals("FALSE",
            Case.of( ( s ) -> "A".equalsIgnoreCase(s), "b" )
                .trueGet( sPrintTrue )
                .falseGet( sPrintFalse ).getResult().get() );

        Assert.assertEquals("FALSE", 
            Case.of( Optional.empty() )
                .trueGet( sPrintTrue )
                .falseGet( sPrintFalse ).getResult().get() );

    }

    @Test
    public void testWithConsumer() {

        Consumer<String> cPrintTrue  = ( s ) -> Assert.assertEquals("TRUE", s );
        Consumer<String> cPrintFalse = ( s ) -> Assert.assertEquals("FALSE", s );

        Case.of( "A".equalsIgnoreCase("a") )
            .trueAccept( cPrintTrue, "TRUE" )
            .falseAccept( cPrintFalse, "FALSE" );

        Case.of( ( s ) -> "A".equalsIgnoreCase(s), "A" )
            .trueAccept( cPrintTrue, "TRUE" )
            .falseAccept( cPrintFalse, "FALSE" );

        Case.of( Optional.of("A") )
            .trueAccept( cPrintTrue, "TRUE" )
            .falseAccept( cPrintFalse, "FALSE" );

        Case.of( "A".equalsIgnoreCase("b") )
            .trueAccept( cPrintTrue, "TRUE" )
            .falseAccept( cPrintFalse, "FALSE" );

        Case.of( ( s ) -> "A".equalsIgnoreCase(s), "b" )
            .trueAccept( cPrintTrue, "TRUE" )
            .falseAccept( cPrintFalse, "FALSE" );

        Case.of( Optional.empty() )
            .trueAccept( cPrintTrue, "TRUE" )
            .falseAccept( cPrintFalse, "FALSE" );
    }

    @Test
    public void testWithBiConsumer() {

        BiConsumer<String,String> cPrintTrue  = ( s1, s2 ) ->
            Assert.assertEquals("TRUE", String.format("%s%s", s1,s2) );

        BiConsumer<String,String> cPrintFalse = ( s1, s2 ) -> 
            Assert.assertEquals("FALSE", String.format("%s%s", s1,s2) );

        Case.of( "A".equalsIgnoreCase("a") )
            .trueAccept( cPrintTrue, "TR","UE" )
            .falseAccept( cPrintFalse, "FA","LSE" );

        Case.of( ( s ) -> "A".equalsIgnoreCase(s), "A" )
            .trueAccept( cPrintTrue, "TR","UE" )
            .falseAccept( cPrintFalse, "FA","LSE" );

        Case.of( Optional.of("A") )
            .trueAccept( cPrintTrue, "TR","UE" )
            .falseAccept( cPrintFalse, "FA","LSE" );

        Case.of( "A".equalsIgnoreCase("b") )
            .trueAccept( cPrintTrue, "TR","UE" )
            .falseAccept( cPrintFalse, "FA","LSE" );

        Case.of( ( s ) -> "A".equalsIgnoreCase(s), "b" )
            .trueAccept( cPrintTrue, "TR","UE" )
            .falseAccept( cPrintFalse, "FA","LSE" );

        Case.of( Optional.empty() )
            .trueAccept( cPrintTrue, "TR","UE" )
            .falseAccept( cPrintFalse, "FA","LSE" );
    }

    @Test
    public void testWithFunction() {

        Function<String, String> fPrintTrue  = ( s ) -> s;
        Function<String, String> fPrintFalse = ( s ) -> s;

        Assert.assertEquals("TRUE", 
            Case.of( "A".equalsIgnoreCase("a") )
                .trueApply( fPrintTrue, "TRUE" )
                .falseApply( fPrintFalse, "FALSE" ).getResult().get() );

        Assert.assertEquals("TRUE",
            Case.of( ( s ) -> "A".equalsIgnoreCase(s), "A" )
                .trueApply( fPrintTrue, "TRUE" )
                .falseApply( fPrintFalse, "FALSE" ).getResult().get() );

        Assert.assertEquals("TRUE", 
            Case.of( Optional.of("A") )
                .trueApply( fPrintTrue, "TRUE" )
                .falseApply( fPrintFalse, "FALSE" ).getResult().get() );

        Assert.assertEquals("FALSE",
            Case.of( "A".equalsIgnoreCase("b") )
                .trueApply( fPrintTrue, "TRUE" )
                .falseApply( fPrintFalse, "FALSE" ).getResult().get() );

        Assert.assertEquals("FALSE",
            Case.of( ( s ) -> "A".equalsIgnoreCase(s), "b" )
                .trueApply( fPrintTrue, "TRUE" )
                .falseApply( fPrintFalse, "FALSE" ).getResult().get() );

        Assert.assertEquals("FALSE", 
            Case.of( Optional.empty() )
                .trueApply( fPrintTrue, "TRUE" )
                .falseApply( fPrintFalse, "FALSE" ).getResult().get() );
    }

    @Test
    public void testWithBiFunction() {

        BiFunction<String, String, String> fPrintTrue  = ( s1, s2 ) -> String.format("%s%s", s1, s2);
        BiFunction<String, String, String> fPrintFalse = ( s1, s2 ) -> String.format("%s%s", s1, s2);

        Assert.assertEquals("TRUE",
            Case.of( "A".equalsIgnoreCase("a") )
                .trueApply( fPrintTrue, "TR","UE" )
                .falseApply( fPrintFalse, "FA","LSE" ).getResult().get() );

        Assert.assertEquals("TRUE", 
            Case.of( ( s ) -> "A".equalsIgnoreCase(s), "A" )
                .trueApply( fPrintTrue, "TR","UE" )
                .falseApply( fPrintFalse, "FA","LSE" ).getResult().get() );

        Assert.assertEquals("TRUE", 
            Case.of( Optional.of("A") )
                .trueApply( fPrintTrue, "TR","UE" )
                .falseApply( fPrintFalse, "FA","LSE" ).getResult().get() );

        Assert.assertEquals("FALSE", 
            Case.of( "A".equalsIgnoreCase("b") )
                .trueApply( fPrintTrue, "TR","UE" )
                .falseApply( fPrintFalse, "FA","LSE" ).getResult().get() );

        Assert.assertEquals("FALSE", 
            Case.of( ( s ) -> "A".equalsIgnoreCase(s), "b" )
                .trueApply( fPrintTrue, "TR","UE" )
                .falseApply( fPrintFalse, "FA","LSE" ).getResult().get() );

        Assert.assertEquals("FALSE",
            Case.of( Optional.empty() )
                .trueApply( fPrintTrue, "TR","UE" )
                .falseApply( fPrintFalse, "FA","LSE" ).getResult().get() );
    }
}