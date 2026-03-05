# LispBM Display Library

The display extensions contains a graphics library designed for platforms for with very limited memory resources. The drawing routines in the library operate on rectangular images (arrays) of pixels , called an image buffer. 

The values stored in image buffers represents colors via an encoding determined by the image buffer pixel format. A pixel buffer has one of the following formats: 

   - indexed2 : 2 colors (1 bit per pixel)
   - indexed4 : 4 colors (2 bits per pixel)
   - indexed16 : 16 colors (4 bits per pixel)
   - rgb332 : 8Bit color
   - rgb565 : 16bit color
   - rgb888 : 24bit color

Note that the RAM requirement of a 100x100 image is; 

   - at indexed2: 1250 Bytes
   - at indexed4: 2500 Bytes
   - at indexed16: 5000 Bytes
   - at rgb332: 10000 Bytes
   - at rgb565: 20000 Bytes
   - at rgb888; 30000 Bytes

So on an embedded platform you most likely not be able to be working with rgb565, rgb888 other than in very limited areas. 

At the low-level end of things you will want to display graphics onto an display. The interface towards the low-level end needs to be implemented for the particular hardware platform and display. For examples of this see [vesc_express](https://github.com/vedderb/vesc_express/tree/main/main/display). The LBM linux REPL has SDL and png backends for the display library. 

the display library is specifically designed to allow for using many colors simultaneously on screen, without needing to use full screen high-color buffers. This is done by delaying the choice of color mapping in the `indexed2`, `indexed4` and `indexed16` images until they are presented on screen. 

images are rendered onto a display using the function `disp-render`. `disp-render` takes an image, a position (x,y) where to draw the image, and a colormapping that can be expressed as a list of colors. for example: 


# Reference


### disp-render

An image is drawn onto a display using `disp-render`. The form of a `disp-render` expression is `(disp-render image x y color-list)`. 

|Arg || 
 |----|----|
 `image`      | An image buffer for example created using img-buffer.
 `x y`        | position of top left corner x,y.
 `color-list` | List of Color value, hex or color values.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(disp-render llama-bin 10 10 '(0x000000 0xFFFFFF))
```


</td>
<td>

<img src=./images/disp-img1.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render llama-bin 20 20 '(0x000000 0xFF0000))
```


</td>
<td>

<img src=./images/disp-img2.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render llama-bin 30 30 '(0x000000 0x00FF00))
```


</td>
<td>

<img src=./images/disp-img3.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render llama-bin 30 30 '(0x000000 0x0000FF))
```


</td>
<td>

<img src=./images/disp-img4.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 0 0 '(0x000000 0xFFFFFF))
```


</td>
<td>

<img src=./images/disp-img5.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 0 100 '(0x000000 0xFF0000))
```


</td>
<td>

<img src=./images/disp-img6.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 100 0 '(0x000000 0x00FF00))
```


</td>
<td>

<img src=./images/disp-img7.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 100 100 '(0x000000 0x0000FF))
```


</td>
<td>

<img src=./images/disp-img8.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 200 0 '(0x000000 0x00FFFF))
```


</td>
<td>

<img src=./images/disp-img9.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-render img-100-100 200 100 '(0x000000 0xFF00FF))
```


</td>
<td>

<img src=./images/disp-img10.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### disp-render-jpg

There is a renderer specifically for JPG images. If one is considering to use JPG for images, the images are most likely larger than what is easy to handle with image buffers. The `disp-render-jpg` decompresses a JPG in small chunks and outputs directly to the display. The form of a `disp-render-jpg` expression is `(disp-render-jpg jpg-data x y)`. 

|Arg || 
 |----|----|
 `jpg-data`   | Imported or otherwise loaded jpg data.
 `x y`        | position of top left corner x,y.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(disp-render-jpg my-jpg 0 0)
```


</td>
<td>

<img src=./images/disp-img11.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### disp-clear

Use `disp-clear` to clear the display
. The form of a `disp-clear` expression is `(disp-clear opt-color)`. `opt-color` is an optional color value. 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(disp-clear)
```


</td>
<td>

<img src=./images/disp-img12.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-clear 0xFF8822)
```


</td>
<td>

<img src=./images/disp-img13.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(disp-clear 0x000000)
```


</td>
<td>

<img src=./images/disp-img14.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### disp-reset

Use `disp-reset` to reset the display
. What it means to reset a display is display-backend dependend on an display connected over SPI to an MCU, it may mean powercycling and running the initialization sequence of commands. The form of a `disp-reset` expression is `(disp-reset)`. 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(disp-reset)
```


</td>
<td>

<img src=./images/disp-img15.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-buffer

Allocate an image buffer from lbm memory or from a compactable region. The form of an `img-buffer` expression is `(img-buffer opt-dm format width height)`. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(define my-img (img-buffer 'indexed2 320 200))
```


</td>
<td>

```clj
[1 64 0 200 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define my-dm (dm-create 10000))
(define my-img (img-buffer my-dm 'indexed2 320 200))
```


</td>
<td>


```clj
[1 64 0 200 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 
```


</td>
</tr>
</table>




---


### img-buffer?

Checks if the argument is likely to be an image buffer. 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-buffer? llama-bin)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-buffer? 'apa)
```


</td>
<td>

```clj
nil
```


</td>
</tr>
</table>




---


### img-blit

```clj
 (img-blit dest src x y transparent ..option)
``` 

Copy pixels from `src` to `dest`.   `x` and `y` are coordinates in `dest`. Pixels colored `transparent` in `src` will be skipped `transparent` can be set to `-1` to indicate no transparency 

|Options||
 |----|----|
 `'(rotate x y deg)` | Rotate `deg` degrees around `x` `y`
 `'(scale s)` | Scale by `s`
 `'(tile)` | Tile to fill `dest`
 `'(clip x y w h)`  | Clip output in destination coords 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1)
```


</td>
<td>

<img src=./images/disp-img16.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1 '(rotate 128 128 45))
```


</td>
<td>

<img src=./images/disp-img17.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1 '(scale 0.500000f32))
```


</td>
<td>

<img src=./images/disp-img18.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1 '(tile) '(scale 0.200000f32))
```


</td>
<td>

<img src=./images/disp-img19.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1
    '(tile)
    '(scale 0.2)
    '(rotate 10 10 45))
```


</td>
<td>

<img src=./images/disp-img20.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-blit my-img llama-bin 10 10 -1
    '(tile)
    '(scale 0.2)
    '(rotate 10 10 45)
    '(clip 50 50 250 150))
```


</td>
<td>

<img src=./images/disp-img21.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-dims

`img-dims returns the width and height of an image. The form of an `img-dims` expression is `(img-dims image)`. 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-dims my-img)
```


</td>
<td>

<img src=./images/disp-img22.png >

</td>
<td>

```clj
(320 200)
```


</td>
</tr>
</table>




---


### img-arc

Draw an arc into an image. The form of an `img-arc` expression is `(img-arc image cx cy r ang-s ang-e color ..option)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `cx cy` | Center point x,y.
 `r`     | Radius.
 `ang-s ang-e` | From angle `ang-s` to `ang-e` in degrees (float).
 `color` | Color value, range determined by image buffer color depth.
 

<br> 

|Option      || 
 |----|----|
 `dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.
 `filled`     | Filled, no arguments.
 `thickness`  | Thickness of line, one argument specifying thickness in pixels.
 `rounded`    | Rounded edges, no argument.
 `resolution` | One argument, Number of points that are connected into an arc using line segments.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1)
```


</td>
<td>

<img src=./images/disp-img23.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(dotted 15 15))
```


</td>
<td>

<img src=./images/disp-img24.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img25.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(thickness 10))
```


</td>
<td>

<img src=./images/disp-img26.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(rounded))
```


</td>
<td>

<img src=./images/disp-img27.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(dotted 15 15) '(resolution 3))
```


</td>
<td>

<img src=./images/disp-img28.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-arc my-img 100 100 50 160 100 1 '(thickness 10) '(rounded))
```


</td>
<td>

<img src=./images/disp-img29.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-circle

Draw a circle into an image. The form of an `img-circle` expression is `(img-circle image cx cy r color ..option)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `cx cy` | Center point x,y.
 `r`     | Radius.
 `color` | Color value, range determined by image buffer color depth.
 

<br> 

|Option      || 
 |----|----|
 `dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.
 `filled`     | Filled, no arguments.
 `thickness`  | Thickness of line, one argument specifying thickness in pixels.
 `resolution` | One argument, Number of points that are connected into an arc using line segments.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1)
```


</td>
<td>

<img src=./images/disp-img30.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1 '(thickness 5))
```


</td>
<td>

<img src=./images/disp-img31.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1 '(dotted 14 14))
```


</td>
<td>

<img src=./images/disp-img32.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img33.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle my-img 100 100 80 1 '(dotted 14 14) '(resolution 6))
```


</td>
<td>

<img src=./images/disp-img34.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-circle-sector

Draw a circle sector into an image. The form of an `img-circle-sector` expression is `(img-circle-sector image cx cy r ang-s ang-e color ..option)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `cx cy` | Center point x,y.
 `r`     | Radius.
 `ang-s ang-e` | From angle `ang-s` to `ang-e` in degrees (float).
 `color` | Color value, range determined by image buffer color depth.
 

<br> 

|Option      || 
 |----|----|
 `dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.
 `filled`     | Filled, no arguments.
 `thickness`  | Thickness of line, one argument specifying thickness in pixels.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-circle-sector my-img 220 40 40 90 200 1)
```


</td>
<td>

<img src=./images/disp-img35.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle-sector my-img 220 40 40 90 200 1 '(thickness 3))
```


</td>
<td>

<img src=./images/disp-img36.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle-sector my-img 220 40 40 90 200 1 '(dotted 1 4))
```


</td>
<td>

<img src=./images/disp-img37.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle-sector my-img 220 40 40 90 200 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img38.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-circle-segment

Draw a circle segment into an image. The form of an `img-circle-segment` expression is `(img-circle-segment image cx cy r ang-s ang-e color ..option)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `cx cy` | Center point x,y.
 `r`     | Radius.
 `ang-s ang-e` | From angle `ang-s` to `ang-e` in degrees (float).
 `color` | Color value, range determined by image buffer color depth.
 

<br> 

|Option      || 
 |----|----|
 `dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.
 `filled`     | Filled, no arguments.
 `thickness`  | Thickness of line, one argument specifying thickness in pixels.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-circle-segment my-img 100 100 80 0 100 1)
```


</td>
<td>

<img src=./images/disp-img39.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle-segment my-img 100 100 80 0 100 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img40.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle-segment my-img 100 100 80 0 100 1 '(thickness 5))
```


</td>
<td>

<img src=./images/disp-img41.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-circle-segment my-img 100 100 80 0 100 1 '(dotted 1 4))
```


</td>
<td>

<img src=./images/disp-img42.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-clear

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-clear my-img)
```


</td>
<td>

<img src=./images/disp-img43.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-clear my-img 1)
```


</td>
<td>

<img src=./images/disp-img44.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-clear my-img 0)
```


</td>
<td>

<img src=./images/disp-img45.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-color

img-color is used to create more complex color objects for use together with disp-render. 

   - **gradient_x**: vertical gradients from color1 to color2.
   - **gradient_y**: horizontal gradients from color1 to color2.
   - **gradient_x_pre**: precomputes gradient.
   - **gradient_y_pre**: precomputes gradient.

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-color 'regular 0xAABB11)
```


</td>
<td>

```clj
[0 67 79 76 17 187 170 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(img-color 'gradient_x color1 color2 10 0 'repeat)
```


</td>
<td>

```clj
[0 67 79 76 0 0 255 0 255 0 0 0 10 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
```


</td>
</tr>
<tr>
<td>

```clj
(img-color 'gradient_x_pre color1 color2)
```


</td>
<td>

```clj
[0 67 79 76 0 0 255 0 255 0 0 0 0 0 0 0 0 0 0 0 3 0 0 0 100 9 249 232]
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define fptr (fopen "images/lama2.bin" "r"))
(define pic (load-file fptr))
(fclose fptr)
(define c (img-color 'gradient_x color1 color2 100 0 'repeat))
(define img (img-buffer 'indexed2 320 200))
(img-blit img pic 10 10 -1 '(rotate 128 128 45))
(disp-render img 100 0 (list (img-color 'regular 0) c))
```


</td>
<td>

<img src=./images/disp-img46.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define fptr (fopen "images/lama2.bin" "r"))
(define pic (load-file fptr))
(fclose fptr)
(define c (img-color 'gradient_y color1 color2 100 0 'mirrored))
(define img (img-buffer 'indexed2 320 200))
(img-blit img pic 10 10 -1 '(rotate 128 128 45))
(disp-render img 100 0 (list (img-color 'regular 0) c))
```


</td>
<td>

<img src=./images/disp-img47.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>




---


### img-color-set

With `img-color-set`you can set properties of a color. The form of a img-color-set expression is `(img-color-set color prop value)` 

|Arg || 
 |----|----|
 `color` | Color value creted with img-color.
 `property` | Symbol denoting property to change.
 `value`    | New value to set property to.
 

|`img-color-set` | regular | gradient | pre |
 |----|----|----|----|
 `color0`      | ✓ | ✓ | ✗ |
 `color1`      | ✗ | ✓ | ✗ |
 `width`       | ✗ | ✓ | ✗ |
 `offset`      | ✗ | ✓ | ✓ |
 `repeat-type` | ✗ | ✓ | ✓ |
 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-color-set my-color 'repeat-type 'mirrored)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-set my-color 'color-0 16711935)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-set my-color 'color-1 65280)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-set my-color 'width 10)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-set my-color 'offset 1)
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>



---


### img-color-get

With `img-color-get` you can access properties of a color. The form of an img-color-get expression is `(img-color-get color prop)` 

|Arg || 
 |----|----|
 `color` | Color value creted with img-color.
 `property` | Symbol denoting property to access.
 

|`img-color-get` | regular | gradient | pre |
 |----|----|----|----|
 `color0`      | ✓ | ✓ | ✓ |
 `color1`      | ✗ | ✓ | ✓ |
 `width`       | ✗ | ✓ | ✓ |
 `offset`      | ✗ | ✓ | ✓ |
 `repeat-type` | ✗ | ✓ | ✓ |
 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-color-get my-color 'repeat-type)
```


</td>
<td>

```clj
mirrored
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-get my-color 'color-0)
```


</td>
<td>

```clj
16711935u32
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-get my-color 'color-1)
```


</td>
<td>

```clj
65280u32
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-get my-color 'width)
```


</td>
<td>

```clj
10i32
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-get my-color 'offset)
```


</td>
<td>

```clj
1i32
```


</td>
</tr>
</table>



---


### img-color-setpre

Update a value in a precalculated gradient color. The form of an `img-color-setpre` expression is `(img-color-setpre color pos color-val)`. 

|Arg || 
 |----|----|
 `color` | Color value creted with img-color.
 `pos`   | Position in the precomputed colormap to update.
 `color-val` | Color value to write into the position.
 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-color-setpre my-color-pre 10 16777215)
```


</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-setpre my-color-pre 11 0)
```


</td>
<td>

```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define fptr (fopen "images/lama2.bin" "r"))
(define pic (load-file fptr))
(fclose fptr)
(define c (img-color 'gradient_x_pre color1 color2 100 0 'repeat))
(loopfor i 0 (< i 512) (+ i 2)
         (progn 
             (img-color-setpre c i 16777215)
             (img-color-setpre c (+ i 1) 0)))
(define img (img-buffer 'indexed2 320 200))
(img-blit img pic 10 10 -1 '(rotate 128 128 45))
(disp-render img 100 0 (list (img-color 'regular 0) c))
```


</td>
<td>

<img src=./images/disp-img48.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define fptr (fopen "images/lama2.bin" "r"))
(define pic (load-file fptr))
(fclose fptr)
(define c (img-color 'gradient_y_pre color1 color2 200 0 'repeat))
(loopfor i 0 (< i 200) (+ i 10)
         (progn 
             (var band-color (if (= (mod (/ i 10) 2) 0) color1 color2))
             (loopfor j 0 (< j 10) (+ j 1)
                      (progn 
                          (img-color-setpre c (+ i j) band-color)))))
(define img (img-buffer 'indexed2 320 200))
(img-blit img pic 10 10 -1 '(rotate 128 128 45))
(disp-render img 100 0 (list (img-color 'regular 0) c))
```


</td>
<td>

<img src=./images/disp-img49.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>



---


### img-color-getpre

Get a value from a precalculated gradient color. The form of an `img-color-getpre` expression is `(img-color-getpre color pos)`. 

|Arg || 
 |----|----|
 `color` | Color value creted with img-color.
 `pos`   | Position in the precomputed colormap to update.
 

<table>
<tr>
<td> Example </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-color-getpre my-color-pre 10)
```


</td>
<td>

```clj
16777215u32
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-getpre my-color-pre 11)
```


</td>
<td>

```clj
0u32
```


</td>
</tr>
<tr>
<td>

```clj
(img-color-getpre my-color-pre 12)
```


</td>
<td>

```clj
16121865u32
```


</td>
</tr>
</table>



---


### img-line

Draw a line into an image. The form of an `img-line` expression is `(img-line image x1 y1 x2 y2 color ..option)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `x1 y1` | Start point  x,y.
 `x2 y2` | End point  x,y.
 `color` | Color value, range determined by image buffer color depth.
 

<br> 

|Option      || 
 |----|----|
 `dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.
 `filled`     | Filled, no arguments.
 `thickness`  | Thickness of line, one argument specifying thickness in pixels.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-line my-img 0 0 320 200 1)
```


</td>
<td>

<img src=./images/disp-img50.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-line my-img 0 200 320 0 1 '(thickness 5))
```


</td>
<td>

<img src=./images/disp-img51.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-line my-img 0 0 320 200 1 '(dotted 4 20))
```


</td>
<td>

<img src=./images/disp-img52.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-rectangle

Draw a rectangle into an image. The form of an `img-rectangle` expression is `(img-rectangle image x1 y1 w h color ..option)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `x1 y1` | Top left corner  x,y.
 `w h`   | Width and height.
 `color` | Color value, range determined by image buffer color depth.
 

<br> 

|Option      || 
 |----|----|
 `dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.
 `filled`     | Filled, no arguments.
 `thickness`  | Thickness of line, one argument specifying thickness in pixels.
 `rounded`    | Rounded edges, one argument rounded corner angle.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-rectangle my-img 10 10 120 180 1)
```


</td>
<td>

<img src=./images/disp-img53.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-rectangle my-img 10 10 120 180 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img54.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-rectangle my-img 10 10 120 180 1 '(rounded 45))
```


</td>
<td>

<img src=./images/disp-img55.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-setpix

Draw a pixel into an image. The form of an `img-setpix` expression is `(img-setpix image x y color)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `x y`   | Position  x,y.
 `color` | Color value, range determined by image buffer color depth.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-setpix my-img 10 10 1)
```


</td>
<td>

<img src=./images/disp-img56.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-getpix

Get a pixel value from an image. The form of an `img-getpix` expression is `(img-getpix image x y)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `x y`   | Position  x,y.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-getpix my-img 10 10)
```


</td>
<td>

<img src=./images/disp-img57.png >

</td>
<td>

```clj
0u32
```


</td>
</tr>
</table>




---


### img-text

Draw text into an image. The form of an `img-text` expression is `(img-text image x1 y1 fg bg font)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `x1 y1` | Position  x,y.
 `fg bg` | Foreground and Background color.
 `font` | font to use. This should be a bin type font as created by for example VESC Tool. The font file can be `imported` or loaded depending on platform
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-text my-img 40 40 1 0 font "LispBM")
```


</td>
<td>

<img src=./images/disp-img58.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-text my-img 40 120 1 0 font "LispBM" 'up)
```


</td>
<td>

<img src=./images/disp-img59.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-text my-img 40 40 1 0 font "LispBM" 'down)
```


</td>
<td>

<img src=./images/disp-img60.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### img-triangle

Draw a triangle into an image. The form of an `img-triangle` expression is `(img-triangle image x1 y1 x2 y2 x3 y3 color ..option)`. 

|Arg || 
 |----|----|
 `image` | An image buffer for example created using img-buffer.
 `x1 y1` | Position first point  x,y.
 `x2 y2` | Position second point  x,y.
 `x3 y3` | Position third point  x,y.
 `color` | Color value, range determined by image buffer color depth.
 

<br> 

|Option      || 
 |----|----|
 `dotted`     | Dotted or dashed, two numeric arguments specifying length of dash and distance between dashes.
 `filled`     | Filled, no arguments.
 `thickness`  | Thickness of line, one argument specifying thickness in pixels.
 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>

```clj
(img-triangle my-img 30 60 160 120 10 180 1)
```


</td>
<td>

<img src=./images/disp-img61.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-triangle my-img 30 60 160 120 10 180 1 '(thickness 5))
```


</td>
<td>

<img src=./images/disp-img62.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-triangle my-img 30 60 160 120 10 180 1 '(filled))
```


</td>
<td>

<img src=./images/disp-img63.png >

</td>
<td>

```clj
t
```


</td>
</tr>
<tr>
<td>

```clj
(img-triangle my-img 30 60 160 120 10 180 1 '(dotted 14 14))
```


</td>
<td>

<img src=./images/disp-img64.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---

# Examples

These examples are leaving out the details on how to setup and initialize any particular display you may have connected to your embedded system. For information on how to initialize a display on a VESC EXPRESS platform see [vesc_express display documentation](https://github.com/vedderb/vesc_express/tree/main/main/display). 


### Example: Sierpinski triangle

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define w 320)
(define h 200)
(define corners (list (cons 10 (- h 10)) (cons (- w 10) (- h 10)) (cons (/ w 2) 10)))
(define s-img (img-buffer 'indexed2 w h))
(defun point (p)
  (img-setpix s-img (car p) (cdr p) 1))
(defun mid-point (p1 p2)
  (progn 
      (let ((x (/ (+ (car p1) (car p2)) 2))
            (y (/ (+ (cdr p1) (cdr p2)) 2)))
           (cons x y))))
(defun sierp (n corners p)
  (if (= n 0) nil (let ((i (mod (rand) 3))
                        (target (ix corners i))
                        (mid (mid-point p target)))
                       (progn 
                           (point mid)
                           (sierp (- n 1) corners mid)))))
(sierp 25000 corners (car corners))
(disp-render s-img 0 0 '(0 16777215))
```


</td>
<td>

<img src=./images/disp-img65.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>




---


### Example: rotated llama

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(import "images/lama2.bin" 'pic)
(define img (img-buffer 'indexed2 320 200))
(img-blit img pic 10 10 -1 '(rotate 128 128 45))
(disp-render img 0 0 '(0 16711680))
```


</td>
<td>

<img src=./images/disp-img66.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>

Note that `import` is a feature of the VESC integration of LispBM and not really a part of core LispBM. The LispBM REPL does not have an import feature currently. 

In the "Desktop" LispBM REPL the rotated llama examples looks as follows. 

<table>
<tr>
<td> Example </td> <td> Image </td> <td> Result </td>
</tr>
<tr>
<td>


```clj
(define pic (load-file (fopen "images/lama2.bin" "r")))
(define img (img-buffer 'indexed2 320 200))
(img-blit img pic 10 10 -1 '(rotate 128 128 45))
(disp-render img 100 0 '(0 16711680))
```


</td>
<td>

<img src=./images/disp-img67.png >

</td>
<td>


```clj
t
```


</td>
</tr>
<tr>
<td>


```clj
(disp-clear)
(define pic (load-file (fopen "images/lama2.bin" "r")))
(define img128x128 (img-buffer 'indexed2 128 128))
(img-blit img128x128 pic 0 0 -1 '(scale 0.500000f32) '(rotate 128 128 45))
(disp-render img128x128 10 10 '(0 16711680))
(img-clear img128x128)
(img-blit img128x128 pic 0 0 -1 '(scale 0.500000f32) '(rotate 128 128 -45))
(disp-render img128x128 148 10 '(0 65280))
```


</td>
<td>

<img src=./images/disp-img68.png >

</td>
<td>


```clj
t
```


</td>
</tr>
</table>

<table>
<tr>
<td> Example </td> <td> Animation </td>
</tr>
<tr>
<td>


```clj
(define pic (load-file (fopen "images/lama2.bin" "r")))
(define img (img-buffer 'indexed2 128 128))
(define m (/ 360.000000f32 100.000000f32))
(disp-clear)
(loopfor i 0 (< i 100) (+ i 1)
         (progn 
             (var rot (list 'rotate 128 128 (* i m)))
             (img-blit img pic 0 0 -1 '(scale 0.500000f32) rot)
             (disp-render img 10 10 '(0 16711680))))
```


</td>
<td>

<img src=./images/disp-anm1.gif >

</td>
</tr>
</table>




---

This document was generated by LispBM version 0.36.0 

