(* handles exporting and display of images and conversion of lists to images *)

(* converts [0,1]%real colorspace to [0,255]%int colors *)

#use "util.ml";;

let floatToCol v = Float.round (255.0 *. v)

let concreteColor = map (map (mapTriple floatToCol))

(* converts an integer into a list of bytes *)
let rec toBytes n i = 
  match n with 
    | 0 -> [] 
    | _ -> (i mod 256) :: toBytes (n-1) (i / 256)
(* converts a byte list to a string for writing into a file *)
let toByteString n = implode << (map Char.chr) << (toBytes n)
(* creates a byte string out of an rgb pixel tuple *)
let pixelToString (a,b,c) = toByteString 1 c ^ toByteString 1 b ^ toByteString 1 a

(* exports an image (as list of lists of pixel tuples) to the bmp format *)
let saveBMP filename img =
  let outchan = open_out filename in
  let output = Printf.fprintf outchan "%s" in

  let h = List.length img in
  let w = List.length (hd img) in
  if (w*3) mod 4 <> 0 then raise Domain else
  (
      output ("BM");(* BM *)
      output (toByteString 4 (w*h*3+54));(* Size *)
      output ("0000"); (* 0000 *)
      output (toByteString 4 54);(* 54 *)
      output (toByteString 4 40);(* 40 *)
      output (toByteString 4 w);(* width *)
      (* normally ~h, but we want the y-axis mirrored *)
      output (toByteString 4 h);(* height *)
      output (toByteString 2 1); (* 00 *)
      output (toByteString 2 24);(* colordepth *)
      output (toByteString 4 0);(* compression *)
      output (toByteString 4 0);(* size *)
      output (toByteString 4 0);(* dpi x *)
      output (toByteString 4 0);(* dpi y *)
      output (toByteString 4 0);(* colortable *)
      output (toByteString 4 0);(* count *)
      List.iter (List.iter (fun x -> output (pixelToString x))) img; (* image *)
      close_out outchan
  ) 


let read_ppm filename =
  let ic = open_in filename in
  let line = input_line ic in
  if line <> "P6" then invalid_arg "not a P6 ppm file";
  let line = input_line ic in
  let line =
    try if line.[0] = '#'  (* skip comments *)
    then input_line ic
    else line
    with _ -> line
  in
  let width, height =
    Scanf.sscanf line "%d %d" (fun w h -> (w, h))
  in
  let line = input_line ic in
  if line <> "255" then invalid_arg "not a 8 bit depth image";
  let all_channels =
    let kind = Bigarray.int8_unsigned
    and layout = Bigarray.c_layout
    in
    Bigarray.Array3.create kind layout 3 width height
  in
  let r_channel = Bigarray.Array3.slice_left_2 all_channels 0
  and g_channel = Bigarray.Array3.slice_left_2 all_channels 1
  and b_channel = Bigarray.Array3.slice_left_2 all_channels 2
  in
  for y = 0 to pred height do
    for x = 0 to pred width do
      r_channel.{x,y} <- (input_byte ic);
      g_channel.{x,y} <- (input_byte ic);
      b_channel.{x,y} <- (input_byte ic);
    done;
  done;
  close_in ic;
  List.init height
  (fun y -> 
    List.init width
    (fun x ->
      (r_channel.{x,y}, g_channel.{x,y}, b_channel.{x,y})
    )
  )
  (* (all_channels,
   r_channel,
   g_channel,
   b_channel) *)

let read_image filename =
  if not(Sys.file_exists filename)
  then failwith(Printf.sprintf "the file %s does not exist" filename);
  let ppmFilename = Filename.temp_file "img" ".ppm" in
  (* let cmd = Printf.sprintf "convert \"%s\" ppm:-" filename in
  let ic, oc = Unix.open_process cmd in *)
  let cmd = Printf.sprintf "convert \"%s\" \"%s\"" filename ppmFilename in
  let ret = Sys.command cmd in
  if ret=0 then
    read_ppm ppmFilename
  else
    failwith(Printf.sprintf "the command %s failed" cmd)
;;

let saveImg = saveBMP
;;
(* exporting to arbitrary image formats using imagemagicks convert program *)
exception ConversionError;;
let saveImgGen filename img = 
  let bmpFilename = Filename.temp_file "img" ".bmp" in
  saveBMP bmpFilename img;
  let ret = Sys.command (String.concat " " ["convert";bmpFilename;filename]) in
  if ret=0 then
    Sys.remove bmpFilename
  else raise ConversionError


let zfill s width =
  let to_fill = width - (String.length s) in
  if to_fill <= 0 then s
  else (String.make to_fill '0') ^ s
