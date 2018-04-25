// Name: Harshil Patel
// CS341: Project 6 
// Date: 04/09/2018


module PPMImageLibrary

#light


//
// DebugOutput:
//
// Outputs to console, which appears in the "Output" window pane of
// Visual Studio when you run with debugging (F5).
//
let rec private OutputImage (image:(int*int*int) list list) = 
  match image with
  | [] -> printfn "**END**"
  | hd::tl -> printfn "%A" hd
              OutputImage tl
           
let DebugOutput(width:int, height:int, depth:int, image:(int*int*int) list list) =
  printfn "**HEADER**"
  printfn "W=%A, H=%A, D=%A" width height depth
  printfn "**IMAGE**"
  OutputImage image


//
// TransformFirstThreeRows:
//
// An example transformation: replaces the first 3 rows of the given image
// with a row of Red, White and Blue pixels (go USA :-).
//
let rec BuildRowOfThisColor row color = 
  match row with
  | []     -> []
  | hd::tl -> color :: BuildRowOfThisColor tl color

let TransformFirstThreeRows(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let row1 = List.head image
  let row2 = List.head (List.tail image)
  let row3 = List.head (List.tail (List.tail image))
  let tail = List.tail (List.tail (List.tail image))
  let newRow1 = BuildRowOfThisColor row1 (255,0,0)      // red:
  let newRow2 = BuildRowOfThisColor row2 (255,255,255)  // white:
  let newRow3 = BuildRowOfThisColor row3 (0,0,255)      // blue:
  let newImage = newRow1 :: newRow2 :: newRow3 :: tail
  newImage


//
// WriteP3Image:
//
// Writes the given image out to a text file, in "P3" format.  Returns true if successful,
// false if not.
//
let Flatten (SL:string list) = 
  List.reduce (fun s1 s2 -> s1 + " " + s2) SL

let Image2ListOfStrings (image:(int*int*int) list list) = 
  List.map (fun TL -> List.map (fun (r,g,b) -> r.ToString()+" "+g.ToString()+" "+b.ToString()+" ") TL) image
  |> List.map Flatten

let rec WriteP3Image(filepath:string, width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let L = [ "P3" ] @ 
          [ System.Convert.ToString(width); System.Convert.ToString(height) ] @
          [ System.Convert.ToString(depth) ] @
          (Image2ListOfStrings image)
  System.IO.File.WriteAllLines(filepath, L)
  true  // success

//
// Grayscale:
//
// Converts the image into grayscale and returns the resulting image as a list of lists. 
// Conversion to grayscale is done by averaging the RGB values for a pixel, and then 
// replacing them all by that average. So if the RGB values were 25 75 250, the average 
// would be 116, and then all three RGB values would become 116 — i.e. 116 116 116.
//

//function which calulates the avg of the tuples
let rec _helperAvg (list:(int*int*int) list) (avgList:(int*int*int) list) =
  match list with
  |[] -> List.rev avgList 
  |(a,b,c)::tail -> _helperAvg tail [((a+b+c)/3,(a+b+c)/3,(a+b+c)/3)] @ avgList

//hekper fucntion for return a list of avg og the tuples
let calculateAvgOfTuple list = 
  _helperAvg list []


let rec Grayscale(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  
  //get the helper function to just get the lists inside the list individually
  let newList = List.map(fun x -> calculateAvgOfTuple x) image
  newList


//
// Threshold
//
// Thresholding increases image separation --- dark values become darker and light values
// become lighter.  Given a threshold value in the range 0 < threshold < MaxColorDepth,
// all RGB values > threshold become the max color depth (white) while all RGB values
// <= threshold become 0 (black).  The resulting image is returned.
//

//change the pixel value here
let changeRGB (value:int) (threshold:int) (depth:int) = 
  if(value > threshold) then depth else 0


//get the tuple and send it to a function where it checks for the threshold
let  _calulateThreshold (T:(int*int*int)) (threshold:int) (depth:int) = 
  let (a,b,c) = T
  let newT = ((changeRGB a threshold depth),(changeRGB b threshold depth),(changeRGB c threshold depth))

  newT

//function send the individual tuples to be manipulated
let calulateThreshold list threshold depth =
  let newList = List.map(fun (a,b,c) -> _calulateThreshold (a,b,c) threshold depth ) list

  newList


//main function which calls the ither helper function
let rec Threshold(width:int, height:int, depth:int, image:(int*int*int) list list, threshold:int) = 
  let newList = List.map(fun x -> calulateThreshold x threshold depth) image

  newList

//
// FlipHorizontal:
//
// Flips an image so that what’s on the left is now on the right, and what’s on 
// the right is now on the left. That is, the pixel that is on the far left end
// of the row ends up on the far right of the row, and the pixel on the far right 
// ends up on the far left. This is repeated as you move inwards toward the center 
// of the row.
//

//just reverese the list and we the the horizontally fillped image
let calulateHorizontal x = 
  let list = List.rev x
  list

//main function call the helper function to calulate the required pixels
let rec FlipHorizontal(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let newList = List.map(fun x -> calulateHorizontal x ) image

  newList
  
//
// Zoom:
//
// Zooms the image by the given zoom factor, which is an integer 0 < factor < 5.  
// The function uses the nearest neighbor approach where each pixel P in the original 
// image is replaced by a factor*factor block of P pixels.  For example, if the zoom 
// factor is 4, then each pixel is replaced by a 4x4 block of 16 identical pixels. 
// The nearest neighbor algorithm is the simplest zoom algorithm, but results in 
// jagged images.  The resulting image is returned.
//

//makes copies of the tuples in the list by a facor
let calulateZoom list factor = 
  let newList = List.map(fun (a,b,c) -> List.replicate factor (a,b,c))list

  newList

//zoom here used List.map which maps the calucated pixed xoomed by a certain factor.
//and then makes a copy of the whole list by a certain factor
let rec Zoom(width:int, height:int, depth:int, image:(int*int*int) list list, factor:int) = 
  let newList = List.map(fun x -> calulateZoom x factor ) image
  let ZoomImage = List.map(fun x -> List.replicate factor x)newList

  ZoomImage
  

//
// RotateRight90:
//
// Rotates the image to the right 90 degrees.
//
let rec RotateRight90(width:int, height:int, depth:int, image:(int*int*int) list list) = 
    
 image
