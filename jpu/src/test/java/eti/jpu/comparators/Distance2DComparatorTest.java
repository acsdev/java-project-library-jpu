package eti.jpu.comparators;

import java.util.stream.Stream;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.Assert;
import org.junit.Test;

import eti.jpu.aux.IGeoPoint2D;

/**
 * This comparator has a goal for order list of data by proximity
 */
public class Distance2DComparatorTest {  
    
    private IGeoPoint2D newPoint(Double latitude, Double longitude) {
        return new IGeoPoint2D() {
            @Override
            public Double getLongitude() {
                return longitude;
            }
        
            @Override
            public Double getLatitude() {
                return latitude;
            }
        };
    }
        
    @Test
    public void testProximity() {

        IGeoPoint2D mainPoint = newPoint(-22.8976692, -43.2783246);

        IGeoPoint2D p3 = newPoint(-22.895250,-43.292940);
        IGeoPoint2D p4 = newPoint(-22.853380,-43.247400);
        IGeoPoint2D p1 = newPoint(-22.896910,-43.278650);
        IGeoPoint2D p2 = newPoint(-22.893580,-43.275840);
        
        List<IGeoPoint2D> points = Stream.of(p3, p2, p1, p4).collect(Collectors.toList());

        Collections.sort( points, new Distance2DComparator(mainPoint) );

        Assert.assertTrue( points.get(0) == p1 );
        Assert.assertTrue( points.get(1) == p2 );
        Assert.assertTrue( points.get(2) == p3 );
        Assert.assertTrue( points.get(3) == p4 );

    }
}