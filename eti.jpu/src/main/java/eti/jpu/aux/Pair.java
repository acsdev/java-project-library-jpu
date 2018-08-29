package eti.jpu.aux;

import java.io.Serializable;

/**
 * This is an auxiliary class mabe to represent two objects.
 */
public class Pair <U extends Serializable, V extends Serializable> implements Serializable {

    private static final long serialVersionUID = 1L;

    private U first;

    private V second;

    public Pair(U first, V second) {

        this.first = first;
        this.second = second;
    }

    public U getFirst() {
        return first;
    }

    public V getSecond() {
        return second;
    }

    public Pair<U, V> getPair() {
        return this;
    }


    @Override
    public boolean equals(Object object) {
        if (this == object) {
            return true;
        }
        if (!(object instanceof Pair)) {
            return false;
        }
        
        @SuppressWarnings({"unchecked"})
        final Pair<U,V> other = Pair.class.cast( object );

        if (!(first == null ? other.first == null : first.equals(other.first))) {
            return false;
        }
        if (!(second == null ? other.second == null : second.equals(other.second))) {
            return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        final int PRIME = 37;
        int result = 1;
        result = PRIME * result + ((first == null) ? 0 : first.hashCode());
        result = PRIME * result + ((second == null) ? 0 : second.hashCode());
        return result;
    }
}