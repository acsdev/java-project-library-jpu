package eti.jpu;

import java.time.LocalDate;
import java.time.temporal.TemporalUnit;
import java.util.Optional;

import org.junit.Assert;

import org.junit.Test;

import eti.jpu.aux.Pair;
/**
 * Provides an new way to execute conditional rotines
 */
public class PairTest {
    
    @Test
    public void testPair() {
        
        String text = "DATE";
        LocalDate now = LocalDate.now();

        Pair<String, LocalDate> pair = new Pair<>(text, now);

        Assert.assertEquals(text, pair.getFirst());

        Assert.assertEquals(now, pair.getSecond());

        Assert.assertTrue(pair.equals(new Pair<>(text, now)));

        Assert.assertTrue(pair.hashCode() == new Pair<>(text, now).hashCode());

        Assert.assertFalse(pair.equals(new Pair<>(text, now.plusDays(1))));

        Assert.assertFalse(pair.equals(new Pair<>(text, "STRING")));
    }
}