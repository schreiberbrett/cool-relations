;; cool-relations.scm

;; $ cd ~
;; $ git clone https://github.com/webyrd/miniKanren-with-symbolic-constraints.git
(load "~/miniKanren-with-symbolic-constraints/mk.scm")
;; ^ uncomment if you want to use this version of miniKanren.

;; $ cd ~
;; $ git clone https://github.com/webyrd/CodeFromTheReasonedSchemer2ndEd.git
;; (load "~/CodeFromTheReasonedSchemer2ndEd/trs2-impl.scm")
;; ^ if this is not imported, then we CANNOT use defrel.

;; For a particular graph theory problem, I need to randomly divide a list of n items among 3 buckets. But I had the additional constraint that the first two buckets must be nonempty.

;; In particular, this problem was about how to divide the vertices of an odd cycle within a tree -- some vertices need to go to the odd cycle's parent -- at least one. And at least m vertices need to be divided up among its n children. TODO: This may require more than 3 buckets.

;; But 3 is an arbitrary magic number. Let's start with the simplest example. 0 buckets instead of 3. But now you're asking to divide by zero, so it's better just to move on to the case of 1 bucket. But then they would all be sorted into the same bucket (since we disallow any leftover elements not bucketed).

;; Here was my first attempt.

(define divide-among-1-bucketo (lambda (elements bucket)
  (conde
    ;; If there are no elements, then none can be added into the sole bucket.
    [(== elements '()) (== bucket '())]
    
    
    ;; Nonempty case: Split the elements into first and rest, and also prepare for having a recursive call to divide-among-1-bucketo.
    [(fresh (first rest bucket-containing-rest)
    
      ;; 1. decompose the nonempty elements into its first element and the rest of the elements
      (== `(,first . ,rest) elements)
      
      ;; Skip ahead to the next comment. Then come back here.
      ;; 3. Finally, remember to sort the first element into the sole bucket. Thus ends the definition.
      (== rest `(,first . ,bucket-containing-rest))
      
      ;; 2. Recursively divide the rest of the elements into a bucket.
      (divide-among-1-bucketo elements bucket-containing-rest))])))
      
;; All this ends up doing is copy all the elements into the single bucket. Notice that this is [eta-equivalent] to the equality relation on two lists.

;; A more exciting case is with 2 lists. Now at each step there is an option about which list to allocate the next element into. Notice that this is NOT like the "team captains" picking players.

(define divide-among-2-bucketso (lambda (elements bucket1 bucket2)
  (conde
  
    ;; To be extra careful, let's split among all 3 cases.
  
    ;; If the elements are exhausted, then there is nothing that can be put in either of the buckets. So both buckets must also be empty.
    [(== elements '()) (== bucket1 '()) (== bucket2 '())]
    
    ;; Otherwise the elements are split into the first and rest of the elements.
    
    [(fresh (first rest) (== elements `(,first . ,rest))
        ;; So the program should nondeterministically allocate first either into bucket1 or bucket2.
        (conde
          
          ;; Allocate first into bucket1
          [(fresh (rest-bucket1)
            (== bucket1 `(,first . ,rest-bucket1))
            (divide-among-2-bucketso rest rest-bucket1 bucket2))]
          
          ;; Allocate first into bucket2
          [(fresh (rest-bucket2)
            (== bucket2 `(,first . ,rest-bucket2))
            (divide-among-2-bucketso rest bucket1 rest-bucket2))]))])))
            
;; Here's what the output looks like:

;; > (run 31 (a b c) (divide-among-2-bucketso a b c))
;; (
;; (()            ()            ())
;; ((_0)          (_0)          ())
;; ((_0)          ()            (_0))
;; ((_0 _1)       (_0 _1)       ())
;; ((_0 _1)       (_1)          (_0))
;; ((_0 _1)       (_0)          (_1))
;; ((_0 _1)       ()            (_0 _1))
;; ((_0 _1 _2)    (_0 _1 _2)    ())
;; ((_0 _1 _2)    (_1 _2)       (_0))
;; ((_0 _1 _2)    (_0 _2)       (_1))
;; ((_0 _1 _2)    (_2)          (_0 _1))
;; ((_0 _1 _2)    (_0 _1)       (_2))
;; ((_0 _1 _2)    (_1)          (_0 _2))
;; ((_0 _1 _2)    (_0)          (_1 _2))
;; ((_0 _1 _2)    ()            (_0 _1 _2))
;; ((_0 _1 _2 _3) (_0 _1 _2 _3) ())
;; ((_0 _1 _2 _3) (_1 _2 _3)    (_0))
;; ((_0 _1 _2 _3) (_0 _2 _3)    (_1))
;; ((_0 _1 _2 _3) (_2 _3)       (_0 _1))
;; ((_0 _1 _2 _3) (_0 _1 _3)    (_2))
;; ((_0 _1 _2 _3) (_1 _3)       (_0 _2))
;; ((_0 _1 _2 _3) (_0 _3)       (_1 _2))
;; ((_0 _1 _2 _3) (_3)          (_0 _1 _2))
;; ((_0 _1 _2 _3) (_0 _1 _2)    (_3))
;; ((_0 _1 _2 _3) (_1 _2)       (_0 _3))
;; ((_0 _1 _2 _3) (_0 _2)       (_1 _3))
;; ((_0 _1 _2 _3) (_2)          (_0 _1 _3))
;; ((_0 _1 _2 _3) (_0 _1)       (_2 _3))
;; ((_0 _1 _2 _3) (_1)          (_0 _2 _3))
;; ((_0 _1 _2 _3) (_0)          (_1 _2 _3))
;; ((_0 _1 _2 _3) ()            (_0 _1 _2 _3))

;; Notice that this carries the same semantic meaning as choosing all elements to include bucket1 or to exclude bucket2 from a superset elements.

;; Rethinking about this, it actually feels more like "pulling" elements from a superset into two subsets. But in relational programming we should always prefer additive operations to subtractive ones. So let's rewrite the arguments so that it goes in the order of bucket1, bucket2, elements.

;; But now it becomes clear that the additive name for this operation is some kind of weave. Let's call it weave2o and rewrite the implementation, recurring on two lists l1 and l2 to "weave together" into an output lout.

(define weave2o (lambda (l1 l2 lout)
  (conde
    ;; Let's recur on BOTH l1 and l2, just to be safe and avoid any overlapping conde cases.
    
    ;; In the case where l1 and l2 are both empty, there is nothing that can be weaved into the output. Notice that this is the exact same argument used in our previous "subtractive" relation.
    [(== l1 '()) (== l2 '()) (== lout '())]
    
    ;; Let's think about what happens when l1 is empty but l2 is nonempty. Then the ONLY list that could contribute to "weaving together" to lout would be l1. So it can be plainly stated that, if l1 is empty, then lout equals l2. Notice that this fact doesn't make any claims about the length of l2, so this overlaps with the above case. It will be condensed in the below version of weave2o.
    [(fresh (l1-first l1-rest)
      (== l1 `(,l1-first . ,l1-rest))
      (== l2 '())
      (== lout l1))]
      
    ;; The same is true with the roles of l1 and l2 flipped.
    [(fresh (l2-first l2-rest)
      (== l1 '())
      (== l2 `(,l2-first . ,l2-rest))
      (== lout l2))]
      
    
    ;; The real recursive case to worry about is when both l1 and l2 are nonempty.
    
    [(fresh (l1-first l1-rest l2-first l2-rest)
      (== l1 `(,l1-first . ,l1-rest))
      (== l2 `(,l2-first . ,l2-rest))
      
      ;; prepare for a recursive call to merge2o
      (fresh (lout-first lout-rest)
        (== lout `(,lout-first . ,lout-rest))
            
        ;; make a choice about whether or not to lead lout with l1-first or with l2-first. Then, make a recursive call, either substituting l1 for l1-rest (and essentially "removing" the first element, since its already been used). Or substituting l2 for l2-rest.
        (conde
          [(== lout-first l1-first) (weave2o l1-rest l2 lout-rest)]
          [(== lout-first l2-first) (weave2o l1 l2-rest lout-rest)])))])))

;; > (run 31 (a b c) (weave2o a b c))
;; ...


;; Let's do another attempt at weave2o, taking advantage of a case collapsing.

(define weave2o (lambda (l1 l2 lout)
  ;; Let's try just splitting on l1, rather than splitting on both l1 and l2.
  (conde
    ;; If l1 is empty, then the only thing that could contribute to the "weave" output is l2, regardless of whether l2 is empty or nonempty.
    [(== l1 '()) (== l2 lout)]
    
    ;; If l1 is not empty, then it must be nonempty, and therefore must have a first element.
    [(fresh (l1-first l1-rest) (== l1 `(,l1-first . ,l1-rest))
      
      ;; Now the only recurrence to worry about is whether or not lout starts with l1-first. But how do we provide the alternate, where lout starts with the first element of l2? Since l2 could be empty, it means that we need to split the cases on l2 anyway.
      (conde
      
        ;; If l2 is empty, we can use the same argument as above, but mirrored.
        [(== l2 '()) (== lout l1)]
        
        ;; Otherwise l2 is nonempty, in which case the cases are not different from the earlier implementation, and it seems like this exercise provided nothing new.
        [(fresh (l2-first l2-rest)
          (== l2 `(,l2-first . ,l2-rest))
          (conde
            [(== lout `(,l1-first . ,lout-rest)) (weave2o l1-rest l2 lout-rest)]
            [(== lout `(,l2-first . ,lout-rest)) (weave2o l1 l2-rest lout-rest)]))]))])))


;; Notice in the first attempt of weave2o, there are some fresh variables that never get unified. So really, what's being checked is whether or not l1 and l2 are nonempty.

;; Let's defire relational analogs to Scheme's nil? and pair? to save on semantically unneccesary fresh variables.
(define nilo (lambda (x)
  (== x '())))
  
(define pairo (lambda (x)
  (fresh (first rest) (== x `(,first . ,rest)))))
  
;; Finally I will revisit merge2o one last time, taking advantage of the new list predicates for conciseness, and renaming lout to weave.


(define weave2o (lambda (l1 l2 weave)
  (conde
    ;; Consider all cases to avoid overlap.
    
    ;; If both lists are empty, their weave is also empty.
    [(nilo l1) (nilo l2) (nilo weave)]

    ;; If exactly one of l1 or l2 is nonempty, then that one is the sole contributor to the weave.
    [(pairo l1) (nilo l2) (== weave l1)]
    [(nilo l1) (pairo l2) (== weave l2)]
    
    ;; Otherwise, if they are both nonempty, then both scenarios are equally likely of whether the weave starts with l1's first element or l2's first element.
    [(fresh (l1-first l1-rest l2-first l2-rest weave-first weave-rest)
      (== l1 `(,l1-first . ,l1-rest)) (== l2 `(,l2-first . ,l2-rest))
      
      ;; The weave must also be nonempty to preserve the invariant that (length (concat l1 l2)) == (length weave)
      (== weave `(,weave-first . ,weave-rest))
      
      (conde
        [(== weave-first l1-first) (weave2o l1-rest l2 weave-rest)]
        [(== weave-first l2-first) (weave2o l1 l2-rest weave-rest)]))])))
        ;; TODO: I still might need to come back to this to avoid 2 recursive calls in 2 branches of the conde.
    

;; It is not to difficult to extend this to weave1o and weave3o. I'll start with the easier case of weave1o that's been seen a couple times.

(define weave1o (lambda (l1 weave)
  (conde
    [(nilo l1) (nilo weave)]
    [(pairo l1) (== weave l1)])))
    
;; This is again just checking for equality between l1 and weave. So it can be rewritten as the following.
(define weave1o (lambda (l1 weave)
  (== l1 weave)))
  

;; Now onto the more difficult case of weave3o.
(define weave3o (lambda (l1 l2 l3 weave)
  (conde
    ;; Again, let's recur on all 3 lists just to be safe and avoid overlapping cases.
    
    ;; When they are all empty, the weave must be empty.
    [(nilo  l1) (nilo  l2) (nilo  l3) (== weave '())]
    
    ;; When exactly one list is nonempty, the weave is equal to that nonempty list.
    [(pairo l1) (nilo  l2) (nilo  l3) (== weave l1)]
    [(nilo  l1) (pairo l2) (nilo  l3) (== weave l2)]
    [(nilo  l1) (nilo  l2) (pairo l3) (== weave l3)]
    
    ;; When exactly two of the lists are nonempty, weave3o degrades into weave2o, since the empty list contributes nothing.
    [(nilo  l1) (pairo l2) (pairo l3) (weave2o l2 l3 weave)]
    [(pairo l1) (nilo  l2) (pairo l3) (weave2o l1 l3 weave)]
    [(pairo l1) (pairo l2) (nilo  l3) (weave2o l1 l2 weave)]
    
    ;; Otherwise, all 3 lists are nonempty.
    [(fresh (
      l1-first l1-rest
      l2-first l2-rest
      l3-first l3-rest
      l1-rec l2-rec l3-rec
      weave-first weave-rest)
      
      (== l1 `(,l1-first . ,l1-rest))
      (== l2 `(,l2-first . ,l2-rest))
      (== l3 `(,l3-first . ,l3-rest))
      
      ;; If all 3 lists are nonempty, then the weave must be nonempty as well.
      (== weave `(,weave-first . ,weave-rest))
      
      (conde
        [(== weave-first l1-first) (== l1-rec l1-rest) (== l2-rec l2)      (== l3-rec l3)]
        [(== weave-first l2-first) (== l1-rec l1)      (== l2-rec l2-rest) (== l3-rec l3)]
        [(== weave-first l3-first) (== l1-rec l1)      (== l2-rec l2)      (== l3-rec l3-rest)])
        
       
      ;; Place the recursive call at the bottom using "rec" variables as opposed to making 3 recursive calls, one in each of the conde branches.
      (weave3o l1-rec l2-rec l3-rec weave-rest))])))
  
;; Unfortunately, this definition of weave3o always leaves one of the lists empty. So the search is incomplete.

;; I will try again using the fact that you can weave the first two lists together, and then the third list to their result. Weaving is associative and commutative.
(define weave3o2 (lambda (l1 l2 l3 weave)
  (fresh (weavel1l2)
    (weave2o l1 l2 weavel1l2)
    (weave2o weavel1l2 l3 weave))))

;; This gives the same results. Let's change it up somewhat.
(define weave3o3 (lambda (l1 l2 l3 weave)
  (fresh (weavel2l3)
    (weave2o l2 l3 weavel2l3)
    (weave2o weavel2l3 l1 weave))))

;; weaveo can be the n-ary generalized version.
;; Weave each list l in ls together.
(define weaveo (lambda (ls out)
  (conde
    [(== ls '()) (== out '())]
    [(fresh (singleton) (== ls `(,singleton)) (== out singleton))]
    [(fresh (first second rest out-rec) (== ls `(,first ,second . ,rest))
      (weave2o first out-rec out)
      (weaveo ls out-rec))])))
    

;; Here is a version of odd-cycleo that does not act on an existing graph. It just builds an odd cycle.

(define odd-cycleo (lambda (vertices edges)
  (conde
    ;; Base case: the 3-cycle
    [(fresh (v0 v1 v2)
      (== vertices `(,v2 ,v1 ,v0))
      (== edges `( (,v2 ,v1) (,v1 ,v0) (,v0 ,v2) )))]
    
    ;; Recursive case:  
    [(fresh (u x y v rest-v rest-e)
      (== vertices `(,u ,x ,y ,v . ,rest-v))
      (== edges `( (,u ,x) (,x ,y) (,y ,v) . ,rest-e))
      (odd-cycleo `(,u ,v . ,rest-v) `( (,u ,v) . ,rest-e)))])))


;; Here is the specification for the "root node" of a "simultaneous collapse", which must be a tree rather than a general graph. (Loops cause a contradiction, to be proved).

(define root-collapso (lambda (vertices edges white blue-parents blue-children)
  (pairo blue-parents)
  (pairo blue-children)
  (weave3o white blue-parents blue-children vertices)
  (odd-cycleo vertices edges)))
  

;; The above never seems to give white vertices. Is there a problem with weave3o when nonempty is specified?
(define weave3-l1-nonemptyo (lambda (l1 l2 l3 weave)
  (pairo l1)
  (weave3o l1 l2 l3 weave)))


;; Set relations (for sets of natural numbers)

;; We represent a set of natural numbers as a sorted list of natural numbers.
;; Where a natural number is represented as a list of units.
(define emptyo (lambda (s)
  (== s '())))

(define singletono (lambda (s x)
  (== s `(,x))))

(define zeroo (lambda (nat)
  (== nat '())))
  
(define not-zeroo (lambda (nat)
  (fresh (x) (== nat `(() . ,x)))))

(define plus1o (lambda (nat succ-nat)
  (== `(() . ,nat) succ-nat)))
  
(define pluso (lambda (n m out)
  (conde
    [(zeroo n) (== m out)]
    [(fresh (n-1 out-1)
      (plus1o n-1 n)
      (plus1o out-1 out)
      (pluso n-1 m out-1))])))

(define leqo (lambda (n m)
  (conde
    [(zeroo n)]
    [(fresh (n-1 m-1)
      (plus1o n-1 n)
      (plus1o m-1 m)
      (leqo n-1 m-1))])))

(define less-thano (lambda (n m)
  (conde
    [(zeroo n) (not-zeroo m)]
    [(fresh (n-1 m-1)
      (plus1o n-1 n)
      (plus1o m-1 m)
      (less-thano n-1 m-1))])))

;; insert a number into a sorted list as long as the element is not already in the sorted list
(define inserto (lambda (sorted-list elem out)
  (conde
    [(== sorted-list '()) (== out `(,elem))]
    [(fresh (first sorted-rest inserted-rest)
      (== sorted-list `(,first . ,sorted-rest))
      (conde
        [(less-thano elem first) (== out `(,elem . ,sorted-list))]

        [(less-thano first elem)
          (== out `(,first . ,inserted-rest))
          (inserto sorted-rest elem inserted-rest)]))])))
      
(define compareo (lambda (n m sout)
  (conde
    [(less-thano n m) (== sout 'lt)]
    [(== n m)         (== sout 'eq)]
    [(less-thano m n) (== sout 'gt)])))

(define uniono (lambda (s1 s2 out)
  (conde
    ;; The union of two empty sets is the empty set.
    [(== s1 '()) (== s2 '()) (== out '())]
    
    ;; The union of an empty set and a nonempty set is the nonempty set.
    [(== s1 '()) (pairo s2) (== out s2)]
    [(pairo s1) (== s2 '()) (== out s1)]
    
    ;; But when both sets are non-empty, then they both contain a least element.
    [(fresh (s1-first s1-rest s2-first s2-rest comparison out-first out-rest s1-rec s2-rec)
      
      
      (==  s1 `( ,s1-first .  ,s1-rest))
      (==  s2 `( ,s2-first .  ,s2-rest))
      (== out `(,out-first . ,out-rest))

      ;; This can be thought of as a branching comparison
      (compareo s1-first s2-first comparison)
      (conde

        ;; The following 2 cases mirror each other. If the elements differ, then include the smaller of the two and recurse
        [(== comparison 'lt)
          (== out-first s1-first)
          (== s1-rec s1-rest)
          (== s2-rec s2)]
        
        [(== comparison 'gt)
          (== out-first s2-first)
          (== s1-rec s1)
          (== s2-rec s2-rest)]

        ;; If it was an element common to both sets, then include it only once
        [(== comparison 'eq)
          (== out-first s1-first) ;; or (== out-first s2-first), they mean the same thing here
          (== s1-rec s1-rest)
          (== s2-rec s2-rest)])
      
      (uniono s1-rec s2-rec out-rest))])))


(define intersectiono (lambda (s1 s2 out)
  (conde
    ;; If either set is empty, then the intersection is empty
    [(== s1 '()) (== out '())]
    [(== s2 '()) (== out '())]
    
    ;; Otherwise it must be the case that both sets have a least element
    [(fresh (s1-first s1-rest s2-first s2-rest out-rest s1-rec s2-rec comparison)
      (== s1 `(,s1-first . ,s1-rest))
      (== s2 `(,s2-first . ,s2-rest))
      
      (compareo s1-first s2-first comparison)
      
      (conde
        ;; If their least element is the same, then include it in the intersection
        [(== comparison 'eq)
          (== out `(,s1-first . ,out-rest))
          (== s1-rec s1-rest)
          (== s2-rec s2-rest)]
        
        ;; Otherwise, discard the lesser element. It is known to not be in both sets, otherwise it wolud have been the least element of both.
        [(== comparison 'lt)
          (== s1-rec s1-rest)
          (== s2-rest s2)
          (== out out-rest)]
        
        [(== comparison 'gt)
          (== s1-rec s1)
          (== s2-rec s2-rest)
          (== out out-rest)])

      (intersectiono s1-rec s2-rec out-rest))])))


;; Graph relations



;;;; Equivalence logical blocks.
;; Consider the two relations:
(define foo1o (lambda (x) (== x 7)))
(define bar1o (lambda (x) (== x 7)))

;; These relations are certainly equivalent because they have the exact same defrel body, just two different names. But notice that the following two relations are also equivalent.

(define foo2o (lambda (x) (== x 7)))
(define bar2o (lambda (y) (== y 7)))

;; The name of the variable is arbitrary, and so as long as every symbol used in a first defrel maps to one -- and only one -- corresponding symbol in the secord relation, then the relations are the same.

;; More advanced equivalences.

(define foo3o (lambda (x y)
  (== x 7)
  (== y 'salmon)))
  
(define bar3o (lambda (z w)
  (== w 'salmon)
  (== z 7)))
  
;; The following 2 relations are also equivalent, but only in the broader definition that, given enough time, the result found in one will be found in the other. They will not return the values in the same order when called with `run` in this version of miniKanren.

(define foo4o (lambda (x)
  (conde
    [(== x 'hello)]
    [(== x 'goodbye)])))
    
(define bar4o (lambda (y)
  (conde
    [(== y 'goodbye)]
    [(== y 'hello)])))



    

    
    
    
    
    
    
    
(define mk-assoco (lambda (defrels assocs)
    (conde
        ((== defrels '()) (== assocs '()))
        ((fresh (name args body rest-defrels rest-assocs)
            (== defrels `( (defrel (,name . ,args) . ,body) . ,rest-defrels))
            (== assocs `( ( ,name . (,args . ,body) ) . ,rest-assocs))
            (mk-assoco rest-defrels rest-assocs))))))
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    


