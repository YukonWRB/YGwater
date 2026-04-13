# YOWN master is as expected

    Code
      master
    Output
        YOWN.Code Name Ownership     Region Last.Site.Visit Last.LTC.Download
      1 YOWN-0000 TEST       WRB Whitehorse           45574             45574
        Last.BL.Download Logger.Type Telemetry Publish Last.Sample.Collected
      1            44825     Solinst        NO     YES                 45574
        Data.in.EQWin? #.of.samples.collected Longitude.(decimal.degree)
      1             No                      1                  -134.9627
        Latitude.(decimal.degree)
      1                  60.60692
                                                           General.Notes SARU.CODE
      1 Area adjacent to the MW regraded; new stickup height: 0.30 m AGS        NA
        S&A.CODE Column1
      1       NA      NA

# YOWN logger tracking is as expected

    Code
      tracking
    Output
      [1] YOWN_ID       Logger_Make   Logger_Type   Logger_Model  Logger_Serial
      [6] Deploy_Date   Retrieve_Date
      <0 rows> (or 0-length row.names)

# xle file is as expected

    Code
      xle
    Output
      {xml_document}
      <Body_xle>
      [1] <File_info>\n  <Company/>\n  <LICENCE/>\n  <Date>2020/05/07</Date>\n  <Ti ...
      [2] <Instrument_info>\n  <Instrument_type>LTC_Jr</Instrument_type>\n  <Model_ ...
      [3] <Instrument_info_data_header>\n  <Project_ID>Long_Term_Well_Network</Proj ...
      [4] <Ch1_data_header>\n  <Identification>LEVEL</Identification>\n  <Unit>m</U ...
      [5] <Ch2_data_header>\n  <Identification>TEMPERATURE</Identification>\n  <Uni ...
      [6] <Ch3_data_header>\n  <Identification>CONDUCTIVITY</Identification>\n  <Un ...
      [7] <Data>\n  <Log id="1">\n    <Date>2019/11/20</Date>\n    <Time>12:00:00</ ...

# xle file can be read

    Code
      res
    Output
      # A tibble: 677 x 4
         Time                `Level (m)` `Temperature (°C)` `Conductivity (µS/cm)`
         <dttm>                    <dbl>              <dbl>                  <dbl>
       1 2019-11-20 19:00:00        3.77                0.4                     59
       2 2019-11-21 01:00:00        3.76                0.4                     59
       3 2019-11-21 07:00:00        3.75                0.4                     58
       4 2019-11-21 13:00:00        3.76                0.4                     58
       5 2019-11-21 19:00:00        3.76                0.4                     58
       6 2019-11-22 01:00:00        3.74                0.4                     58
       7 2019-11-22 07:00:00        3.71                0.4                     58
       8 2019-11-22 13:00:00        3.65                0.4                     58
       9 2019-11-22 19:00:00        3.65                0.4                     58
      10 2019-11-23 01:00:00        3.65                0.4                     58
      # i 667 more rows

