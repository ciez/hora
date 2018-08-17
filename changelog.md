#####   2.2.0
    add DatePartSmall
    
#####   2.1.0
    add serializeable UTCTimeBin: close copy of UTCTime. 
    
    UTCTime and UTCTimeBin are easily convertible without loss of precision.   

    fromUTC is now a class function. It may be necessary to specify the result type.  

    Tz derive Eq, Ord

#####   2.0.2
    changes to Data.Time.Hora.Zone:
    
        remove redundant  ToTimeZone UTCTime (Tz (DatePart a)) instance: 
        
        use Data.Time.Hora.Part fromUtc'
        

#####   2.0.1
    include test data files

#####   2.0
    *API changes: modules moved & renamed*
    
    add DatePart -> UTCTime conversion
    
    add TimeZone conversions
    
    add Format type
    
    merge type modules into 1 module : Type


#####   1.1.1
    Bug fix: Ord datePart
    
    Timestamp now -> Num

#####   1.1
    API overhaul
    
    remove dep on regex-do 

#####   1.0.3
    fix Eq, Ord TimeSpan: was derived. Implemented manually
    
    add Num TimeSpan 

#####   1.0.2
    Tz derive Functor
    
    Data.Time.Hora.Future :

        PicoDiff class

#####   1.0.1
    DmyHmp derive Binary

#####   1.0
    initial version
