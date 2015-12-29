let tga f =
  let width = 640 in
  let height = 480 in
  let header =
    [|0;0;2;0;0;0;0;0;0;0;0;0;
      width mod 256;width/256;
      height mod 256;height/256;
      24;0|] in
  let asByte x =
     min 255 (max 0 (int_of_float (128.0 *. (x +. 1.0))))
  in
  let o = open_out_bin "x.tga" in
  Array.iter (output_byte o) header;
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
       let u = float_of_int (x-width/2) /. float_of_int width in
       let v = float_of_int (y-height/2) /. float_of_int height in
       let r,g,b = f (u,v) in
       output_byte o (asByte b);
       output_byte o (asByte g);
       output_byte o (asByte r)
    done
  done;
  close_out o
let () =
   tga (fun (x,y) -> sin x, sin y, x *. y)