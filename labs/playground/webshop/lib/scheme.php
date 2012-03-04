<?php
// (define-alias isnull function "isnull")

// (define-alias reduce function "array_reduce")
// (define-alias slice function "array_slice")

// (define-operator and "and")
// (define-operator or "or")

class Pair { 
  public $car; 
  public $cdr; 
  public function __constructor ($a, $d) { 
    $this->$car = $a; $this->$cdr = $d; return $this; 
  } 
} 

function pair_list_is ($x) { 
  return (($x instanceof Pair) or isnull ($x)); 
} 

function _cons ($a, $d) { return new Pair ($a, $d); } 

function _car ($p) { return $p->$car; } 

function _cdr ($p) { return $p->$cdr; } 

function array__gt_list ($arr) { 
  return array_reduce (
    $arr, function ($ls, $elem) { return _cons ($elem, $ls); }, NULL); 
} 

function cons ($a, $d) { 
  return new Pair ($a, (pair_list_is ($d) ? $d : array__gt_list ($d))); 
} 

// (define-syntax car ((let ((/x/ (app ...))) (if (instanceof /x/ Pair) (slot /x/ car) (vector-ref /x/ 0))) (let ((/x/ (app ...))) (if (instanceof /x/ Pair) (slot /x/ car) (vector-ref /x/ 0)))) ((if (instanceof p Pair) (slot p car) (vector-ref p 0)) (if (instanceof p Pair) (slot p car) (vector-ref p 0))))

// (define-syntax cdr ((if (instanceof p Pair) (slot p cdr) (array->list (slice p 1))) (if (instanceof p Pair) (slot p cdr) (array->list (slice p 1)))))

$_div_x_div__gensym_2 = cons (1, 2); 
if (($_div_x_div__gensym_2 instanceof Pair)) { 
  $_div_x_div__gensym_2->$car; 
} else { 
  $_div_x_div__gensym_2[0]; 
}
?>
