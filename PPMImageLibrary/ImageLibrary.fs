module PPMImageLibrary

#light
//
// F#-based PPM image library.
//
// <<Ativ Aggarwal>>
// U. of Illinois, Chicago
// CS341, Spring 2018
// Project 06
//

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

let hhGscale tup =
    let (a,b,c)= tup
    let avg= (a+b+c)/3
    (avg, avg, avg)

let hGrayscale list1 =
    list1 |> List.map(fun (a,b,c) -> hhGscale (a,b,c))
//
// Grayscale:
//
// Converts the image into grayscale and returns the resulting image as a list of lists. 
// Conversion to grayscale is done by averaging the RGB values for a pixel, and then 
// replacing them all by that average. So if the RGB values were 25 75 250, the average 
// would be 116, and then all three RGB values would become 116 — i.e. 116 116 116.
//
let Grayscale(width:int, height:int, depth:int, image:(int*int*int) list list) = 
    let image1= image |> List.map(fun list -> hGrayscale list)
    image1

//
// Threshold
//
// Thresholding increases image separation --- dark values become darker and light values
// become lighter.  Given a threshold value in the range 0 < threshold < MaxColorDepth,
// all RGB values > threshold become the max color depth (white) while all RGB values
// <= threshold become 0 (black).  The resulting image is returned.
//
let hhhthres a thres=
    match a with
    | a when a<=thres -> 0
    | a when a> thres -> 255

let hhthres tup thres=
    let (a,b,c)= tup
    let a1= hhhthres a thres
    let b1= hhhthres b thres
    let c1= hhhthres c thres
    (a1,b1,c1)

let hThreshold list1 thres=
    list1 |> List.map(fun (a,b,c) -> hhthres (a,b,c) thres)

let Threshold(width:int, height:int, depth:int, image:(int*int*int) list list, threshold:int) = 
    let image1= image |> List.map(fun list -> hThreshold list threshold)
    image1



//
// FlipHorizontal:
//
// Flips an image so that what’s on the left is now on the right, and what’s on 
// the right is now on the left. That is, the pixel that is on the far left end
// of the row ends up on the far right of the row, and the pixel on the far right 
// ends up on the far left. This is repeated as you move inwards toward the center 
// of the row.
//
let hFlip list1 =
    List.rev list1

let FlipHorizontal(width:int, height:int, depth:int, image:(int*int*int) list list) = 
    let image1= image |> List.map(fun list -> hFlip list)
    image1



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
let hhZoom tup factor= 
    let (a,b,c)= tup
    match factor with
    | 1-> [(a,b,c)]
    | 2-> [(a,b,c);(a,b,c)]
    | 3->[(a,b,c);(a,b,c);(a,b,c)]
    | 4->[(a,b,c);(a,b,c);(a,b,c);(a,b,c)]
    | _->[(a,b,c)]

let rec hZoom list factor=
    //list |> List.map(fun (a,b,c) -> hhZoom (a,b,c) factor)
    match list with
    | []->[]
    | hd::tl -> (hhZoom hd factor)@ (hZoom tl factor)

let hcopy var factor=
    match factor with
    | 1-> [var]
    | 2-> [var;var]
    | 3-> [var;var;var]
    | 4-> [var;var;var;var]
    | _-> [var]

let rec copy list factor=
    match list with
    | []->[]
    | hd::tl -> (hcopy hd factor)@ (copy tl factor)

let Zoom(width:int, height:int, depth:int, image:(int*int*int) list list, factor:int) = 
  let image1= image |> List.map(fun list-> hZoom list factor)
  let image2= copy image1 factor
  image2



//
// RotateRight90:
//
// Rotates the image to the right 90 degrees.
//

let rec hRotate90 image =
    match image with
    | row::tl ->
        match row with
        | col::tail ->
            let hd= List.map List.head image
            let hd1= List.rev hd
            let rest= List.map List.tail image
            hd1::hRotate90(rest)
        | _->[]
    | _->[]

let RotateRight90(width:int, height:int, depth:int, image:(int*int*int) list list) = 
  let image1= hRotate90 image
  image1
