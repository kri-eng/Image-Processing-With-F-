//
// F# image processing functions.
//
// The file contains the important functions that will take an image from
// the C# image and then returns the newly craeted the images as per the
// the command of the user.
//
// Krish Patel, UIC, 11/20/22
//

namespace ImageLibrary

module Operations =
  //
  // all functions must be indented
  //

  //
  // _getGrayScaleTruple: The function takes in the pixel with the
  //                      and then returns the calculated average
  //                      as the same rgb values.
  //
  let private _getGrayscaleTruple (r_val:int, g_val:int, b_val:int) = 
    let average = ((float r_val * 0.299) + (float g_val * 0.587) + (float b_val * 0.114))
    (int average, int average, int average)

  //
  // _grayScale: The function takes in the average and then uses the map
  //             function to calcuate the average for each individual
  //             pixel.
  //
  //
  let rec private _grayScale image =
    match image with
      | [] -> []
      | head::tail -> let new_head = List.map (fun x -> _getGrayscaleTruple x) head
                      // Attaches the new head to the next returned image.
                      new_head::(_grayScale tail)

  //
  // Grayscale:
  //
  // Converts the image into grayscale and returns the 
  // resulting image as a list of lists. Pixels in grayscale
  // have the same value for each of the Red, Green and Blue
  // values in the RGB value.  Conversion to grayscale is done
  // by using a WEIGHTED AVERAGE calculation.  A normal average
  // (adding the three values and dividing by 3) is not the best,
  // since the human eye does not perceive the brightness of 
  // red, green and blue the same.  The human eye perceives 
  // green as brighter than red and it perceived red as brighter
  // than blue.  Research has shown that the following weighted
  // values should be used when calculating grayscale.
  //  - the green value should account for 58.7% of the grayscale.
  //  - the red value should account for   29.9% of the grayscale.
  //  - the blue value should account for  11.4% of the grayscale.
  //
  // So if the RGB values were (25, 75, 250), the grayscale amount 
  // would be 80, (25 * 0.299 + 75 * 0.587 + 250 * 0.114 => 80)
  // and then all three RGB values would become 80 or (80, 80, 80).
  // We will use truncation to cast from the floating point result 
  // to the integer grayscale value.
  //
  // Returns: updated image.
  //
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 

    // Call the helper function to get the value.
    _grayScale image  
      

  //
  // _gettingHelper: Takes in the threshold value and rgb_val, return 255
  //                 if the values is greater else return 0.
  //
  //
  let private _gettingHelper t_val rgb_val =
    if rgb_val > t_val then
      255
    else
      0

  //
  // _getThresholdTruple: Gets the t_val and also a truple and calls the
  //                      helper function for each individual pixel value
  //                      i.e. r,g and b. Returns a new truple at the end.
  //
  //
  let private _getThresholdedTruple t_val (r_val:int, g_val:int, b_val:int) =
    let new_r_val = _gettingHelper t_val r_val
    let new_g_val = _gettingHelper t_val g_val
    let new_b_val = _gettingHelper t_val b_val
    (new_r_val, new_g_val, new_b_val)
    

  //
  // _thresHold: The fucntion takes in the image and then
  //             call the helper function and returns a new image.
  //
  //
  let rec private _thresHold t_val image =
    match image with
      | [] -> []
      | head::tail -> let new_head = List.map (fun x -> _getThresholdedTruple t_val x) head
                      new_head::(_thresHold t_val tail)               


  //
  // Threshold
  //
  // Thresholding increases image separation --- dark values 
  // become darker and light values become lighter. Given a 
  // threshold value in the range 0 < threshold < color depth,
  // each RGB value is compared to see if it's > threshold.
  // If so, that RGB value is replaced by the color depth;
  // if not, that RGB value is replaced with 0. 
  //
  // Example: if threshold is 100 and depth is 255, then given 
  // a pixel (80, 120, 160), the new pixel is (0, 255, 255).
  //
  // Returns: updated image.
  //
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    // for now, just return the image back, i.e. do nothing:
    _thresHold threshold image


  //
  // __FlipHorizontal: Takes in the row and returns a new row with calling
  //                   the same function with tail. and return a flipped     //                   list.
  //
  //
  let rec private __FlipHorizontal row new_row =
    match row with
      | [] -> new_row
      | head::tail -> __FlipHorizontal tail (head::new_row)

  //
  // _FlipHorizontal: Takes in the image and calls the helper function for
  //                  the individual lists.
  // 
  //
  let rec private _FlipHorizontal image =
    match image with
      | [] -> []
      | head::tail -> let new_head = __FlipHorizontal head []
                      new_head::(_FlipHorizontal tail)


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    _FlipHorizontal image


  //
  //
  // _calculateEdge: Takes in the three truples and calculating the
  //                 distance between the two pixels. If the value is
  //                 greater than the threshold then return appropriate
  //                 color pixel.
  //
  let private _calculateEdge (r_val, g_val, b_val) (r_val_r, g_val_r, b_val_r) (r_val_b, g_val_b, b_val_b) t_val =
    let val1 = (sqrt (float ((r_val - r_val_r)*(r_val - r_val_r) + (g_val - g_val_r)*(g_val - g_val_r) + (b_val - b_val_r)*(b_val - b_val_r))))
    let val2 = (sqrt (float ((r_val - r_val_b)*(r_val - r_val_b) + (g_val - g_val_b)*(g_val - g_val_b) + (b_val - b_val_b)*(b_val - b_val_b))))

    //
    //
    // Comparing the val1 and t_val and returning appropriate results.
    if val1 > float t_val then
      (0, 0, 0)
    else if val2 > float t_val then
      (0, 0, 0)
    else
      (255, 255, 255)


  //
  // _edgeDetectionHelper: The fucntion takes in the values and sends
  //                       the appropriate values to the next helper
  //                       function
  //
  //
  let rec private _edgeDetectionHelper row1 row2 width threshold =
    match row1, row2 with
      | hd1::tl1, hd2::tl2 when (width > 1) -> let hd12 = List.head tl1
                                               let new_head = _calculateEdge hd1 hd12 hd2 threshold
                                               new_head::(_edgeDetectionHelper tl1 tl2 (width-1) threshold)
      | _, _ -> []

  // let rec private _edgeDetectionHelper row1 row2 threshold =
  //   match row1 row2 with
  //     | [], [] -> []
  //     | _, [] -> []
  //     | [], _ -> []
  //     | hd1::tl1::[], hd2::tl2::[] -> []
  //     | hd1::tl1, hd2::tl2 -> let nextCell = List.head tl1
  //                             let new_head = _calculateEdge hd1 nextCell hd2 threshold
  //                             new_head::(_edgeDetectionHelper tl1 tl2 threshold)

  //
  // _edgeDetection: Takes in the image, width, height and threshold.
  //                 The function gets the first row, second row untill
  //                 the height is eqaul to one. It takes the rows and
  //                 calls the helper function.
  //
  //
  let rec private _edgeDetection image width height threshold = 
    match image with
      | [] -> []
      | head::tail when (height > 1) -> let row2 = List.head tail
                                        let new_head = _edgeDetectionHelper head row2 width threshold
                                        new_head::(_edgeDetection tail width (height - 1) threshold)
      | _ -> []

  // let rec private _edgeDetection image threshold =
  //   match image with
  //     | [] -> []
  //     | head::tail::[] -> []
  //     | head::tail -> let bottomRow = List.head tail
  //                     let new_head = _edgeDetectionHelper head bottomRow threshold
  //                     new_head::(_edgeDetection tail threshold)

  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "signigicantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compares each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    // for now, just return the image back, i.e. do nothing:
    _edgeDetection image threshold    

    
  // 
  // ___rotate90: The funciton takes in the row and the expetecWIdth
  //              as well as the currWidth, it then return sthe head when
  //              expected index is equal to curr index.
  //
  //
  let rec private ___rotate90 row expectedWidth currWidth =
    if currWidth = expectedWidth then
      let head = List.head row
      head
    else
      let tail = List.tail row
      ___rotate90 tail expectedWidth (currWidth+1)


  //
  // __rotate90: The function takes in the head and then calls the helper 
  //             function with the head and the index we need with the
  //             it is recursing on. and then calls itself recursively.
  //
  // 
  let rec private __rotate90 image currWidth totalWidth = 
    match image with
      | [] -> []
      | head::tail -> (___rotate90 head currWidth 1)::(__rotate90 tail currWidth totalWidth)


  //
  // _rotate90: The fucntion takes in a the image and totalidth with the
  //            current index that we are get the heads of. Then return
  //            the newly created image.
  //
  //
  let rec private _rotate90 image totalWidth currWidth =
    if currWidth > totalWidth then
      []
    else
      (__rotate90 image currWidth totalWidth)::(_rotate90 image totalWidth (currWidth + 1))

  //
  // RotateRight90:
  //
  // Rotates the image to the right 90 degrees.
  //
  // Returns: updated image.
  //
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    // for now, just return the image back, i.e. do nothing:
    FlipHorizontal width height depth (_rotate90 image width 1)

