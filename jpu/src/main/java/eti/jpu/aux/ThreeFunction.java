package eti.jpu.aux;

/**
 * Function interface that has a method to work with three arguments.
  */
@FunctionalInterface
public interface ThreeFunction<T, U, V, R> {    
    
    /**
     * Method that receive three arguments and return something.
     * lambda sintax is like this (a1, a2, a3) -> r;
     * 
     * @param t &lt;T&gt; the first function argument.
     * @param u &lt;U&gt; the second function argument.
     * @param v &lt;V&gt; the third function argument.
     * 
     * @return &lt;R&gt; return type.
     */
    R apply(T t, U u, V v);
}