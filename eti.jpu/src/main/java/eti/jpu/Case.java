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
     */
    public static Case of(boolean value) {
        return new Case(value);
    }
    
    /**
     * Create a case using predicate and value to test with the predicate.
     * 
     * @param value predicate of something
     * @param objectValue will be tested with predicate to decide result of the case
     */
    public static <T> Case of(Predicate<T> value, T objectValue) {
        Objects.requireNonNull( value );
        
        return new Case( value.test( objectValue ) );
    }

    /**
     * Create a case using optional
     * 
     * @param optional The result of the case is decided by optional.isPresent() 
     */
    public static Case of(Optional<?> optional) {
        return new Case( optional.isPresent() );
    }

    /**
     * Execute a runnble method if the value of Case is <code>true</code>.
     * @param runnable Runnable instance that will be executed if Case is true.
     */
    public TrueOption<?> trueRun(Runnable runnable) {
        Objects.requireNonNull( runnable );        
        if (Boolean.TRUE.equals( objectValue )) {
            runnable.run();
        }
        return new TrueOption<>();
    }
    
    public <R> TrueOption<R> trueGet(Supplier<R> supplier) {
        Objects.requireNonNull( supplier );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.ofNullable( supplier.get() );
        }
        
        return new TrueOption<R>();
    }
    
    public <E> TrueOption<Consumer<E>> trueAccept(Consumer<E> consumer) {
        Objects.requireNonNull( consumer );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.of( consumer );
        }
        
        return new TrueOption<Consumer<E>>();
    }
    
    public <E> TrueOption<?> trueAccept(Consumer<E> consumer, E element) {
        Objects.requireNonNull( consumer );
        
        if (Boolean.TRUE.equals( objectValue )) {
            consumer.accept( element );
        }
        
        return new TrueOption();
    }
    
    public <E,F> TrueOption<BiConsumer<E,F>> trueAccept(BiConsumer<E,F> biConsumer) {
        Objects.requireNonNull( biConsumer );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.of( biConsumer );
        }
        
        return new TrueOption<BiConsumer<E,F>>();
    }
    
    public <E,F> TrueOption<?> trueAccept(BiConsumer<E,F> biConsumer, E elementOne, F elementTwo) {
        Objects.requireNonNull( biConsumer );
        
        if (Boolean.TRUE.equals( objectValue )) {
            biConsumer.accept( elementOne, elementTwo );
        }
        
        return new TrueOption();
    }
    
    public <E, R> TrueOption<Function<E, R>> trueApply(Function<E, R> mapper) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper );
        }
        
        return new TrueOption<Function<E, R>>();
    }
    
    public <E, F, R> TrueOption<BiFunction<E, F, R>> trueApply(BiFunction<E, F, R> mapper) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper );
        }
        
        return new TrueOption<BiFunction<E, F, R>>();
    }
    
    public <E, R> TrueOption<R> trueApply(Function<E, R> mapper, E element) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper.apply( element ) );
        }
        
        return new TrueOption<R>();
    }
    
    public <E, F, R> TrueOption<R> trueApply(BiFunction<E, F, R> mapper, E elementOne, F elementTwo) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.TRUE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper.apply( elementOne, elementTwo ) );
        }
        
        return new TrueOption<R>();
    }
    
    public FalseOption<?> falseRun(Runnable runnable) {
        Objects.requireNonNull( runnable );
        
        if (Boolean.FALSE.equals( objectValue )) {
            runnable.run();
        }
        
        return new FalseOption();
    }
    
    public <R> FalseOption<R> falseGet(Supplier<R> supplier) {
        Objects.requireNonNull( supplier );
        
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.ofNullable( supplier.get() );
        }
        
        return new FalseOption<R>();
    }
    
    public <E> FalseOption<Consumer<E>> falseAccept(Consumer<E> consumer) {
        Objects.requireNonNull( consumer );
        
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.of( consumer );
        }
        
        return new FalseOption<Consumer<E>>();
    }
    
    public <E> FalseOption<?> falseAccept(Consumer<E> consumer, E element) {
        Objects.requireNonNull( consumer );
        
        if (Boolean.FALSE.equals( objectValue )) {
            consumer.accept( element );
        }
        
        return new FalseOption();
    }
    
    public <E, F> FalseOption<BiConsumer<E, F>> falseAccept(BiConsumer<E, F> biConsumer) {
        Objects.requireNonNull( biConsumer );
        
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.of( biConsumer );
        }
        
        return new FalseOption<BiConsumer<E, F>>();
    }
    
    public <E, F> FalseOption<?> falseAccept(BiConsumer<E, F> biConsumer, E elementOne, F elementTwo) {
        Objects.requireNonNull( biConsumer );
        
        if (Boolean.FALSE.equals( objectValue )) {
            biConsumer.accept( elementOne, elementTwo );
        }
        
        return new FalseOption();
    }
    
    public <E, R> FalseOption<Function<E, R>> falseApply(Function<E, R> mapper) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper );
        }
        
        return new FalseOption<Function<E, R>>();
    }
    
    public <E, F, R> FalseOption<BiFunction<E, F, R>> falseApply(BiFunction<E, F, R> mapper) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper );
        }
        
        return new FalseOption<BiFunction<E, F, R>>();
    }
    
    public <E, F, R> TrueOption<R> falseApply(BiFunction<E, F, R> mapper, E elementOne, F elementTwo) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper.apply( elementOne, elementTwo ) );
        }
        
        return new TrueOption<R>();
    }
    
    public <E, R> FalseOption<R> falseApply(Function<E, R> mapper, E element) {
        Objects.requireNonNull( mapper );
        
        if (Boolean.FALSE.equals( objectValue )) {
            this.result = Optional.ofNullable( mapper.apply( element ) );
        }
        
        return new FalseOption<R>();
    }
    
    public class TrueOption<T> extends ResultOption<T> {
        
        public ResultOption<?> falseRun(Runnable runnable) {
            Objects.requireNonNull( runnable );
            
            if (Boolean.FALSE.equals( objectValue )) {
                runnable.run();
            }
            
            return new ResultOption();
        }
        
        public <R> ResultOption<R> falseGet(Supplier<R> supplier) {
            Objects.requireNonNull( supplier );
            
            if (Boolean.FALSE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( supplier.get() );
            }
            
            return new ResultOption<R>();
        }
        
        public <E> ResultOption<Consumer<E>> falseAccept(Consumer<E> consumer) {
            Objects.requireNonNull( consumer );
            
            if (Boolean.FALSE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( consumer );
            }
            
            return new ResultOption<Consumer<E>>();
        }
        
        public <E> ResultOption<?> falseAccept(Consumer<E> consumer, E element) {
            Objects.requireNonNull( consumer );
            
            if (Boolean.FALSE.equals( objectValue )) {
                consumer.accept( element );
            }
            
            return new ResultOption();
        }
        
        public <E, F> ResultOption<?> falseAccept(BiConsumer<E, F> biConsumer, E elementOne, F elementTwo) {
            Objects.requireNonNull( biConsumer );
            
            if (Boolean.FALSE.equals( objectValue )) {
                biConsumer.accept( elementOne, elementTwo );
            }
            
            return new ResultOption();
        }
        
        public <E, F> ResultOption<BiConsumer<E, F>> falseAccept(BiConsumer<E, F> biConsumer) {
            Objects.requireNonNull( biConsumer );
            
            if (Boolean.FALSE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( biConsumer );
            }
            
            return new ResultOption<BiConsumer<E, F>>();
        }
        
        public <E, R> ResultOption<Function<E, R>> falseApply(Function<E, R> mapper) {
            Objects.requireNonNull( mapper );
            
            if (Boolean.FALSE.equals( Case.this.objectValue )) {
                Case.this.result = Optional.ofNullable( mapper );
            }
            
            return new ResultOption<Function<E, R>>();
        }
        
        public <E, F, R> ResultOption<BiFunction<E, F, R>> falseApply(BiFunction<E, F, R> mapper) {
            Objects.requireNonNull( mapper );
            
            if (Boolean.FALSE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( mapper );
            }
            
            return new ResultOption<BiFunction<E, F, R>>();
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
        
        public <E> ResultOption<Consumer<E>> trueAccept(Consumer<E> consumer) {
            Objects.requireNonNull( consumer );
            
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                Case.this.result = Optional.ofNullable( consumer );
            }
            
            return new ResultOption<Consumer<E>>();
        }
        
        public <E> ResultOption<T> trueAccept(Consumer<E> consumer, E element) {
            Objects.requireNonNull( consumer );
            
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                consumer.accept( element );
            }
            
            return new ResultOption<T>();
        }
        
        public <E,F> ResultOption<BiConsumer<E,F>> trueAccept(BiConsumer<E,F> biConsumer) {
            Objects.requireNonNull( biConsumer );
            
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                Case.this.result = Optional.ofNullable( biConsumer );
            }
            
            return new ResultOption<BiConsumer<E,F>>();
        }
        
        public <E,F> ResultOption<T> trueAccept(BiConsumer<E,F> biConsumer, E elementOne, F elementTwo) {
            Objects.requireNonNull( biConsumer );
            
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                biConsumer.accept( elementOne, elementTwo );
            }
            
            return new ResultOption<T>();
        }
        
        public <E, R> ResultOption<Function<E, R>> trueApply(Function<E, R> mapper) {
            Objects.requireNonNull( mapper );
            
            if (Boolean.TRUE.equals( Case.this.objectValue )) {
                Case.this.result = Optional.ofNullable( mapper );
            }
            
            return new ResultOption<Function<E, R>>();
        }
        
        public <E, F, R> ResultOption<BiFunction<E, F, R>> trueApply(BiFunction<E, F, R> mapper) {
            Objects.requireNonNull( mapper );
            
            if (Boolean.TRUE.equals( objectValue )) {
                Case.this.result = Optional.ofNullable( mapper );
            }
            
            return new ResultOption<BiFunction<E, F, R>>();
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
    
    public class ResultOption<T> {
        @SuppressWarnings({ "unchecked", "cast" })
        public Optional<T> getResult() {
            return (Optional<T>) Case.this.result;
        }
    }
}