package eti.jpu.comparators;


import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.Assert;
import org.junit.Test;

import eti.jpu.aux.Pair;


public class NumberLevelComparatorTest {      
   
    @Test
    public void testOrderedString() {
        
        List<String> numbers = Stream.of("0.1.0", "1.0.1","2.0.1","0.3.0.1","4", "1.0","2.0.3").collect(Collectors.toList());

        // HERE MAGIC GOES
        Collections.sort( numbers, new NumberLevelComparator<String>() );

        Assert.assertTrue( "0.1.0".equals(numbers.get(0)) ); 
        
        Assert.assertTrue( "4".equals(numbers.get(6)) );
    }


    @Test
    public void testOrderedObject() {
        
        List<Pair<String, String>> numbers = new ArrayList<>();

        numbers.add(new Pair<>("ZERO.ONE.ZERO","0.1.0"));
        numbers.add(new Pair<>("ONE.ZERO.ONE","1.0.1"));
        numbers.add(new Pair<>("TWO.ZERO.ZERO","2.0.1"));
        numbers.add(new Pair<>("ZERO.THREE.ZERO.ONE","0.3.0.1"));
        numbers.add(new Pair<>("FOUR","4"));
        numbers.add(new Pair<>("ONE.ZERO","1.0"));
        numbers.add(new Pair<>("TWO.ZERO.THREE","2.0.3"));

        // HERE MAGIC GOES
        Collections.sort( numbers, new NumberLevelComparator<Pair<String, String>>( p -> p.getSecond() ) );

        Assert.assertTrue( "ZERO.ONE.ZERO".equals(numbers.get(0).getFirst()) ); 
        
        Assert.assertTrue( "FOUR".equals(numbers.get(6).getFirst()) );
    }
}