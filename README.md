# Java pure utils (jpu)

This project has a single goal, became an big repository of smart algorithims in java, without any dependencies (except junit).

### Rules
* All code must have javadoc (in english preferably);
* All code must have unit test;
* All code must be free of any third library;

### Recommendations
* Prefer to code in english
* After send an code, put small description on README.me file.

### Sumary of util classes
|Class|Description|
|---:|---|
|eti.jpu.Case|Provide an fluid way to execute conditional rotines|
|eti.jpu.comparators.Distance2DComparator|This comparator has a goal for order list of data by proximity (latitude and longitude)|
|eti.jpu.comparators.NumberLevelComparator|This comparator has a goal to order data like 1, 1.1, 2, 3, 3.1, 4. etc|

### Sumary of auxliary classes
|Class|Description|
|---:|---|
|eti.jpu.aux.Pair|This is an auxiliary class mabe to represent two objects|
|eti.jpu.aux.IGeoPoint2D|Represents a single geograhfic point (latitude / longitude)|
|eti.jpu.aux.ThreeConsumer|A consumer to work with three arguments|
|eti.jpu.aux.ThreeFunction|A function to work with three arguments|

#### Useful information
To generate javadoc use __mvn clean javadoc:javadoc__ on pom.xml. A directory named __javadoc__ will be create in your main directory with complete javadoc documentation.

To generate report about the code, use verify phase. Just run __mvn clean verify__ on pom.xml. In target/report-codes/pmd you will find pmd report in case the code has pmd violation. In target/report-codes/jacoco will you find a report that shows how much of tne code is covered by unit test.


#### Feel free to jump in [;-[)