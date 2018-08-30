package eti.jpu.comparators;

import java.util.Comparator;
import java.util.function.ToDoubleFunction;

import eti.jpu.aux.IGeoPoint2D;

import java.util.logging.Logger;

/**
 * This comparator has a goal for order list of data by proximity (latitude and longitude).
 */
public class Distance2DComparator implements Comparator<IGeoPoint2D> {
    
    private static Logger logger = Logger.getLogger(Distance2DComparator.class.getName());

    private IGeoPoint2D mainPoint;

    /**
     * Constructor that receives mainPoint.
     * @param mainPoint reference point, means that other point will be ordered by proximity of mainPoint
     */
    public Distance2DComparator(IGeoPoint2D mainPoint) {
        this.mainPoint = mainPoint;
    }

    /**
     * Method responsavel for compare distances
     */
    private Double compareDistance(IGeoPoint2D otherPoint) {
        try {
            ToDoubleFunction<Double> deg2rad = deg -> deg * Math.PI / 180.0;
            ToDoubleFunction<Double> rad2deg = rad -> rad * 180.0 / Math.PI;

            if ( otherPoint == null || mainPoint == null ) {
                return 1D;
            }            
            if ( otherPoint.getLatitude() == null || otherPoint.getLongitude() == null || mainPoint.getLatitude() == null || mainPoint.getLongitude() == null ) {
                return 1D;
            }
            
            double theta    = otherPoint.getLongitude() - mainPoint.getLongitude();
            
            double distLAT  = Math.sin(deg2rad.applyAsDouble(otherPoint.getLatitude())) * Math.sin(deg2rad.applyAsDouble(mainPoint.getLatitude()));

            double distLong = Math.cos(deg2rad.applyAsDouble(otherPoint.getLatitude())) * Math.cos(deg2rad.applyAsDouble(mainPoint.getLatitude()));

            double dist     =  distLAT + distLong * Math.cos(deg2rad.applyAsDouble(theta));
            
            return rad2deg.applyAsDouble( Math.acos(dist) );
            
        } catch ( Exception ex ) {
            logger.severe( ex.getMessage() );
            return 1D;
        }
    }   

    /**
     * @param p1
     * @param p2
     * @return int
     * @see java.util.Comparator#compare
     */
    @Override
    public int compare(IGeoPoint2D p1, IGeoPoint2D p2) {
        return compareDistance(p1).compareTo(compareDistance(p2));
    }
}