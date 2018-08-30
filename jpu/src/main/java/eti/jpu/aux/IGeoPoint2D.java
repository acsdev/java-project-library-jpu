package eti.jpu.aux;

/**
 * Represents a single geograhfic point (latitude / longitude)
 */
public interface IGeoPoint2D {
    /**
     * Get latutide
     * @return double value as latutide
     */
    Double getLatitude();

    /**
     * Get longitude
     * @return double value as longitude
     */
    Double getLongitude();
}