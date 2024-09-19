# LispBM Display Reference Manual


### img-buffer

Allocate an image buffer from lbm memory or from a compactible region. The form of an `img-buffer` expression is `(img-buffer opt-dm format width height)`. 

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
[1 64 0 200 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
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
[1 64 0 200 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
```


</td>
</tr>
</table>




---


### arcs

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

<img src=./images/img1.png >

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

<img src=./images/img2.png >

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

<img src=./images/img3.png >

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

<img src=./images/img4.png >

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

<img src=./images/img5.png >

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
(img-arc my-img 100 100 50 160 100 1 '(dotted 15 15) '(resolution 3))
```


</td>
<td>

<img src=./images/img6.png >

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

<img src=./images/img7.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### circles

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

<img src=./images/img8.png >

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

<img src=./images/img9.png >

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

<img src=./images/img10.png >

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

<img src=./images/img11.png >

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
(img-circle my-img 100 100 80 1 '(dotted 14 14) '(resolution 6))
```


</td>
<td>

<img src=./images/img12.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### circle sectors

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

<img src=./images/img13.png >

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

<img src=./images/img14.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### circle segments

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

<img src=./images/img15.png >

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

<img src=./images/img16.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---


### lines

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

<img src=./images/img17.png >

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
(img-line my-img 0 200 320 0 1 '(dotted 4 20))
```


</td>
<td>

<img src=./images/img18.png >

</td>
<td>

```clj
t
```


</td>
</tr>
</table>




---

