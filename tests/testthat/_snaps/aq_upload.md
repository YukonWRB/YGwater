# aquarius upload works without overwrite

    Code
      aq_upload("TEST", "Air Temp.Temperature", data, overwrite = FALSE)
    Output
      
       Your request was completed:
      0 points were appended out of the 337 that were in the provided dataset.
      The points were appended to the timeseries Air Temp.Temperature at location TEST
      2023-12-12 18:50:18.007947 UTC
      $appended
      [1] 0
      
      $input
      [1] 337
      

# aquarius upload works with overwrite

    Code
      aq_upload("TEST", "Air Temp.Temperature", data, overwrite = TRUE, start = min(
        sequence), end = max(sequence))
    Output
      
       Your request was completed:
      337 points were appended out of the 337 that were in the provided dataset.
      The points were appended to the timeseries Air Temp.Temperature at location TEST
      2023-12-12 18:51:13.219372 UTC
      $appended
      [1] 337
      
      $input
      [1] 337
      

