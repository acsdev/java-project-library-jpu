package eti.jpu.aux;

/**
 * Function interface that has a method to work with three arguments.
  */
@FunctionalInterface
public interface ThreeConsumer<T, U, V, R> {    
    
    /**
     * Method that receive three arguments and return nothing.
     *
     * @param t &lt;T&gt; the first function argument.
     * @param u &lt;U&gt; the second function argument.
     * @param v &lt;V&gt; the third function argument.
     */
    void apply(T t, U u, V v);
}