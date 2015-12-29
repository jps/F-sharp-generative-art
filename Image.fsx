#r "System.Drawing.dll"
let mutable imageNumber = 0;


open System.Drawing
open System;

// Create bitmap
let width, height = 640, 480
let image = new Bitmap(width, height)
for y = 0 to height - 1 do
   for x = 0 to width - 1 do
      let u = -1.0 + (float x * 2.0) / float width
      let v = -1.0 + (float y * 2.0) / float height
      let r = Math.PI % (Math.Asin u * Math.Sin u) + Math.Ceiling( u ) * Math.PI
      let g = Math.Ceiling v % Math.Ceiling (Math.Cos v) * Math.E
      let b = u * Math.Tan v * Math.PI
      let norm n = int (128.0 * (n + 1.0)) |> min 255 |> max 0
      image.SetPixel(x,y,Color.FromArgb(norm r,norm g,norm b))

// Save bitmap
open System.IO
let filePath = Path.Combine(__SOURCE_DIRECTORY__ , "x.png")
image.Save(filePath, Imaging.ImageFormat.Png)
