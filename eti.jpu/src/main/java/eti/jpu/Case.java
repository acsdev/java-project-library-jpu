package eti.jpu;

import java.util.Objects;
import java.util.Optional;
import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

/**
 * Provides an new way to execute conditional rotines
 */
public final class Case {
    
    private boolean objectValue;
    
    private Optional<?> result = Optional.empty();

    /**
     * @param value resutl of test
     */
    private Case(boolean value) {
        this.objectValue = value;
    }
    
    /**
     * Create a case using a boolean variable
     * 
     * @param value resutl of test
     * @return new instance of Case
     */
    public static Case of(boolean value) {
        return new Case(value);
    }
    
    /**
     * Create a case using predicate and value to test with the predicate.
     * 
     * @param value predicate of something
     * @param objectValue will be tested with predicate to decide result of the case
     * @return new instance of Case
     */
    public static <T> Case of(Predicate<T> value, T objectValue) {
        Objects.requireNonNull( value );
        
        return new Case( value.test( objectValue ) );
    }

    /**
     * Create a case using optional
     * 
     * @param optional The result of the case is decided by optional.isPresent() 
     * @return new instance of Case
     */
    public static Case of(Optional<?> optional) {
        return new Case( optional.isPresent() );
    }

    /**
     * Execute a Runnble if "Case Result" equals true.
     * @param runnable Runnable instance that will be executed if "Case Result" equals true.
     * @return TrueOption&lt;?&gt; class that can be use to configure rotine if "Case Result" equals false.
     */
    public TrueOption<?> trueRun(Runnable runnable) {
        Objects.requireNonNull( runnable );        
        if (Boolean.TRUE.equals( objectValue )) {
            runnable.run();
        }
        return new TrueOption<>();
    }
    
    /**
     * Execute a Supplier if "Case Result" equals true.
     * @param supplier Supplier&lt;R&gt; instance that will be executed if "Case Result" equals true.
     * @return TrueOption&lt;R&gt; class that can be use to configure rotine if "Case Result" equals false.
     */
    public <R> TrueOption<R> trueGet(Supplier<R> supplier) {
        Objects.requireNonNull( supplier );
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.ofNullable( supplier.get() );
        }        
        return new TrueOption<R>();
    }
    
  
    /**
     * Execute a Consumer if "Case Result" equals true.
     * @param consumer Consumer&lt;E&gt; instance that will be executed if "Case Result" equals true.
     * @param element element E that will be used as arg in accept method.
     * @return TrueOption&lt;?&gt; class that can be use to configure rotine if "Case Result" equals false.
     */
    public <E> TrueOption<?> trueAccept(Consumer<E> consumer, E element) {
        Objects.requireNonNull( consumer );
        if (Boolean.TRUE.equals( objectValue )) {
            consumer.accept( element );
        }        
        return new TrueOption<>();
    }
    
    /**
     * Execute a BiConsumer if "Case Result" equals true.
     * @param biConsumer Consumer&lt;E&gt; instance that will be executed if "Case Result" equals true.
     * @param elementOne elementOne E that will be used as first arg in accept method.
     * @param elementTwo elementTwo F that will be used as second arg in accept method.
     * @return TrueOption&lt;?&gt; class that can be use to configure rotine if "Case Result" equals false.
     */
    public <E,F> TrueOption<?> trueAccept(BiConsumer<E,F> biConsumer, E elementOne, F elementTwo) {
        Objects.requireNonNull( biConsumer );
        
        if (Boolean.TRUE.equals( objectValue )) {
            biConsumer.accept( elementOne, elementTwo );
        }
        
        return new TrueOption<>();
    }
    
    /**
     * Execute a Function if "Case Result" equals true.
     * @param mapper Function&lt;E, R&gt; instance that will be executed if "Case Result" equals true.
     * @param element element E that will be used as arg in apply method.
     * @return TrueOption&lt;R&gt; class that can be use to configure rotine if "Case Result" equals false.
     */
    public <E, R> TrueOption<R> trueApply(Function<E, R> mapper, E element) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper.apply( element ) );
        }
        
        return new TrueOption<R>();
    }
    
    /**
     * Execute a BiFunction if "Case Result" equals true.
     * @param mapper Function&lt;E, F, R&gt; instance that will be executed if "Case Result" equals true.
     * @param elementOne element E that will be used as first arg in apply method.
     * @param elementTwo element F that will be used as second arg in apply method.
     * @return TrueOption&lt;R&gt; class that can be use to configure rotine if "Case Result" equals false.
     */
    public <E, F, R> TrueOption<R> trueApply(BiFunction<E, F, R> mapper, E elementOne, F elementTwo) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper.apply( elementOne, elementTwo ) );
        }
        
        return new TrueOption<R>();
    }
    
    /**
     * Execute a Runnble if "Case Result" equals false.
     * @param runnable Runnable instance that will be executed if "Case Result" equals false.
     * @return FalseOption&lt;?&gt; class that can be use to configure rotine if "Case Result" equals true.
     */
    public FalseOption<?> falseRun(Runnable runnable) {
        Objects.requireNonNull( runnable );
        if (Boolean.FALSE.equals( objectValue )) {
            runnable.run();
        }        
        return new FalseOption<>();
    }
    
    /**
     * Execute a Supplier if "Case Result" equals false.
     * @param supplier Supplier&lt;R&gt; instance that will be executed if "Case Result" equals false.
     * @return FalseOption&lt;R&gt; class that can be use to configure rotine if "Case Result" equals true.
     */
    public <R> FalseOption<R> falseGet(Supplier<R> supplier) {
        Objects.requireNonNull( supplier );        
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.ofNullable( supplier.get() );
        }
        return new FalseOption<R>();
    }
    
    /**
     * Execute a Consumer if "Case Result" equals false.
     * @param consumer Consumer&lt;E&gt; instance that will be executed if "Case Result" equals false.
     * @param element element E that will be used as arg in accept method.
     * @return FalseOption&lt;?&gt; class that can be use to configure rotine if "Case Result" equals true.
     */
    public <E> FalseOption<?> falseAccept(Consumer<E> consumer, E element) {
        Objects.requireNonNull( consumer );
        if (Boolean.FALSE.equals( objectValue )) {
            consumer.accept( element );
        }        
        return new FalseOption<>();
    }
     
    /**
     * Execute a BiConsumer if "Case Result" equals false.
     * @param biConsumer BiConsumer&lt;E, F&gt; instance that will be executed if "Case Result" equals false.
     * @param elementOne element E that will be used as first arg in accept method.
     * @param elementTwo element F that will be used as second arg in accept method.
     * @return FalseOption&lt;?&gt; class that can be use to configure rotine if "Case Result" equals true.
     */
    public <E, F> FalseOption<?> falseAccept(BiConsumer<E, F> biConsumer, E elementOne, F elementTwo) {
        Objects.requireNonNull( biConsumer );
        if (Boolean.FALSE.equals( objectValue )) {
            biConsumer.accept( elementOne, elementTwo );
        }
        return new FalseOption<>();
    }
    
    /**
     * Execute a Function if "Case Result" equals false.
     * @param mapper Function&lt;E, R&gt; instance that will be executed if "Case Result" equals false.
     * @param element element E that will be used as arg in apply method.
     * @return FalseOption&lt;R&gt; class that can be use to configure rotine if "Case Result" equals true.
     */
    public <E, R> FalseOption<R> falseApply(Function<E, R> mapper, E element) {
        Objects.requireNonNull( mapper );
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper.apply( element ) );
        }        
        return new FalseOption<R>();
    }
    
    /**
     * Execute a BiFunction if "Case Result" equals false.
     * @param mapper BiFunction&lt;E, F, R&gt; instance that will be executed if "Case Result" equals false.
     * @param elementOne element E that will be used as first arg in apply method.
     * @param elementTwo element F that will be used as second arg in apply method.
     * @return FalseOption&lt;?&gt; class that can be use to configure rotine if "Case Result" equals true.
     */
    public <E, F, R> FalseOption<R> falseApply(BiFunction<E, F, R> mapper, E elementOne, F elementTwo) {
        Objects.requireNonNull( mapper );
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper.apply( elementOne, elementTwo ) );
        }       
        return new FalseOption<R>();
    }
    
    /**
     * Inner class that allows fluid sintax to call rotine that will be used when Case is false.
     */
    public class TrueOption<T> extends ResultOption<T> {
        
        public ResultOption<?> falseRun(Runnable runnable) {
            Objects.requireNonNull( runnable );
            if (Boolean.FALSE.equals( objectValue )) {
                runnable.run();
            }            
            return new ResultOption<>();
        }
        
        public <R> ResultOption<R> falseGet(Supplier<R> supplier) {
            Objects.requireNonNull( supplier );
            if (Boolean.FALSE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( supplier.get() );
            }            
            return new ResultOption<R>();
        }
        
        public <E> ResultOption<?> falseAccept(Consumer<E> consumer, E element) {
            Objects.requireNonNull( consumer );            
            if (Boolean.FALSE.equals( objectValue )) {
                consumer.accept( element );
            }
            return new ResultOption<>();
        }
        
        public <E, F> ResultOption<?> falseAccept(BiConsumer<E, F> biConsumer, E elementOne, F elementTwo) {
            Objects.requireNonNull( biConsumer );
            if (Boolean.FALSE.equals( objectValue )) {
                biConsumer.accept( elementOne, elementTwo );
            }            
            return new ResultOption<>();
        }
        
        public <E, R> ResultOption<R> falseApply(Function<E, R> mapper, E element) {
            Objects.requireNonNull( element );
            if (Boolean.FALSE.equals( Case.this.objectValue )) {
                Case.this.result = Optional.ofNullable( mapper.apply( element ) );
            }            
            return new ResultOption<R>();
        }
        
        public <E, F, R> ResultOption<R> falseApply(BiFunction<E, F, R> mapper, E elementOne, F elementTwo) {
            Objects.requireNonNull( mapper );
            if (Boolean.FALSE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( mapper.apply(elementOne, elementTwo) );
            }
            return new ResultOption<R>();
        }
    }
    
    /**
     * Inner class that allows fluid sintax to call rotine that will be used when Case is true.
     */
    public class FalseOption<T> extends ResultOption<T> {        
        
        public ResultOption<T> trueRun(Runnable runnable) {
            Objects.requireNonNull( runnable );
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                runnable.run();
            }
            return new ResultOption<T>();
        }
        
        public <R> ResultOption<R> trueGet(Supplier<R> supplier) {
            Objects.requireNonNull( supplier );
            if (Boolean.TRUE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( supplier.get() );
            }        
            return new ResultOption<R>();
        }
        
        public <E> ResultOption<T> trueAccept(Consumer<E> consumer, E element) {
            Objects.requireNonNull( consumer );
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                consumer.accept( element );
            }
            return new ResultOption<T>();
        }
        
        public <E,F> ResultOption<T> trueAccept(BiConsumer<E,F> biConsumer, E elementOne, F elementTwo) {
            Objects.requireNonNull( biConsumer );
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                biConsumer.accept( elementOne, elementTwo );
            }
            return new ResultOption<T>();
        }
        
        public <E, R> ResultOption<R> trueApply(Function<E, R> mapper, E element) {
            Objects.requireNonNull( element );
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                Case.this.result = Optional.ofNullable( mapper.apply( element ) );
            }
            return new ResultOption<R>();
        }
        
        public <E, F, R> ResultOption<R> trueApply(BiFunction<E, F, R> mapper, E elementOne, F elementTwo) {
            Objects.requireNonNull( mapper );
            if (Boolean.TRUE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( mapper.apply(elementOne, elementTwo) );
            }
            return new ResultOption<R>();
        }
    }

    /**
     * Auxiliay class to allow fluid sintax.
     */
    public class ResultOption<T> {
        @SuppressWarnings({ "unchecked" })
        public Optional<T> getResult() {
            return (Optional<T>) Case.this.result;
        }
    }
}