# Code Samples

### Rust

```rust
fn main() {
    println!("{}", vec![1, 2, 3, 4, 5].iter().sum::<i32>());
    println!("{}", vec![1, 2, 3, 4, 5].iter().prod::<i32>());
}
```

### Emacs Lisp

```lisp
;; product
(apply '* '(1 2 3 4 5))

;; sum
(apply '+ '(1 2 3 4 5))
```

### APL

```
⍝ Sum...

+/ 1 2 3 4 5
= 15

⍝ Product...

×/ 1 2 3 4 5
= 120
```

### Smalltalk

```smalltalk
#(1 2 3 4 5) fold: [:sum :number | sum + number]
#(1 2 3 4 5) fold: [:product :number | product * number]
```

### JavaScript (ES6)

```js
// sum :: (Num a) => [a] -> a
const sum = xs => xs.reduce((a, x) => a + x, 0);
```

### Ruby

```ruby
[1, 2, 3, 4, 5].sum
```

### Shell

```bash
sum=0
prod=1
list="1 2 3"
for n in $list
do sum="$(($sum + $n))"; prod="$(($prod * $n))"
done
echo $sum $prod
```
### Cobol

```basic
       IDENTIFICATION DIVISION.
       PROGRAM-ID. array-sum-and-product.
 
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       78  Array-Size              VALUE 10.
       01  array-area              VALUE "01020304050607080910".
           03  array               PIC 99 OCCURS Array-Size TIMES.
 
       01  array-sum               PIC 9(8).
       01  array-product           PIC 9(10) VALUE 1.
 
       01  i                       PIC 99.
 
       PROCEDURE DIVISION.
           PERFORM VARYING i FROM 1 BY 1 UNTIL Array-Size < i
               ADD array (i) TO array-sum
               MULTIPLY array (i) BY array-product
           END-PERFORM
 
           DISPLAY "Sum:     " array-sum
           DISPLAY "Product: " array-product
 
           GOBACK
           .
```

### Fortran

```fortran
integer, dimension(10) :: a = (/ (i, i=1, 10) /)
integer :: sresult, presult
 
sresult = sum(a)
presult = product(a)
```

### Java

```java
public class SumProd
   {
       public static void main(final String[] args)
       {
           int sum = 0;
           int prod = 1;
           int[] arg = {1,2,3,4,5};
           for (int i : arg)
               {
                   sum += i;
                   prod *= i;
               }
       }
}
```

