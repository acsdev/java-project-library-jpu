package eti.jpu.comparators;

import java.util.Comparator;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This comparator has a goal to order data like 1, 1.1, 2, 3, 3.1, 4. etc.
 * It is possible order a collection of string or collection of object where the object has attribute or method that returns this kink of data. 
 */
public class NumberLevelComparator<T> implements Comparator<T> {
    
    private static final long FACTOR = 1000000000000L;

    private Function<T, String> transform;

    /**
     * Default constructor
     * When use this constructor that code will assume that element T must be cast by string to get order level number.
     */
    public NumberLevelComparator() {
        transform = ( t ) -> String.class.cast( t );
    }

    /**
     * Construtor with transform function
     * @param transform this function is responsable for recover the order level number of an object with type T.
     */
    public NumberLevelComparator(Function<T, String> transform) {
        this.transform = transform;
    }

    /**
     * @param first
     * @param second
     * @see java.util.Comparator#compare
     */
    @Override
    public int compare(T first, T second) {

        Function<String, String> prepare = (s) -> Stream.of( s.split("\\.") )
            .map( part -> Long.parseLong( part ) + FACTOR)
            .map(String::valueOf)
            .collect(Collectors.joining("."));

        String s1 = prepare.apply( transform.apply( first ) );

        String s2 = prepare.apply( transform.apply( second ) );
        
        return s1.compareTo(s2);
    }
}