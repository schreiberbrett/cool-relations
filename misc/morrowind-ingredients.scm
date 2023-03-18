(load "~/miniKanren-with-symbolic-constraints/mk.scm")

(define riffleo (lambda (a b o)
    (fresh (car-a cdr-a car-b cdr-b car-o cdr-o z0 z1)
        (conde
            ;; If `a` and `b` are both empty, then the output is empty.
            ((== a '()) (== b '()) (== o '()))
            
            ;; If `a` is non-empty and `b` is empty, then the output is equal to `a`.
            ((== a `(,car-a . ,cdr-a)) (== b '()) (== o a))
            
            ;; If `a` is empty and `b` is non-empty, then the output is equal to `b`.
            ((== a '()) (== b `(,car-b . ,cdr-b)) (== o b))
            
            ;; When both `a` and `b` are non-empty
            ((== a `(,car-a . ,cdr-a)) (== b `(,car-b . ,cdr-b)) (== o `(,car-o . ,cdr-o))
                (conde
                    ((== car-o car-a) (== z0 cdr-a) (== z1 b))
                    ((== car-o car-b) (== z0 a) (== z1 cdr-b)))
                    
                (riffleo z0 z1 cdr-o))))))


(define ingredient-table `(
    (alit-hide
        (drain-intelligence resist-poison telekinesis detect-animal))
    (ampoule-pod
        (water-walking paralyze detect-animal drain-willpower))
    (ash-salts
        (drain-agility resist-magicka cure-blight-disease))
    (ash-yam
        (fortify-intelligence fortify-strength resist-common-disease detect-key))
    (bittergreen-petals
        (restore-intelligence invisibility drain-endurance drain-magicka))
    (black-anther
        (drain-agility resist-fire drain-endurance light))
    (black-lichen
        (drain-strength resist-frost drain-speed cure-poison))
    (bloat
        (drain-magicka fortify-intelligence fortify-willpower detect-animal))
    (bonemeal
        (restore-agility telekinesis drain-fatigue drain-personality))
    (bread
        (restore-fatigue))
    (bunglers-bane
        (drain-speed drain-endurance dispel drain-strength))
    (chokeweed
        (drain-luck restore-fatigue cure-common-disease drain-willpower))
    (coda-flower
        (drain-personality levitate drain-intelligence drain-health))
    (comberry
        (drain-fatigue restore-magicka fire-shield reflect))
    (corkbulb-root
        (cure-paralyzation restore-health lightning-shield fortify-luck))
    (corpus-weepings
        (drain-fatigue fortify-luck drain-willpower restore-health))
    (crab-meat
        (restore-fatigue resist-shock lightning-shield restore-luck))
    (daedra-skin
        (fortify-strength cure-common-disease paralyze swift-swim))
    (daedras-heart
        (restore-magicka fortify-endurance drain-agility night-eye))
    (diamond
        (drain-agility invisibility reflect detect-key))
    (dreugh-wax
        (fortify-strength restore-strength drain-luck drain-willpower))
    (ectoplasm
        (fortify-agility detect-animal drain-strength drain-health))
    (emerald
        (fortify-magicka restor-health drain-agility drain-endurance))
    (fire-petal
        (resist-fire drain-health spell-absorption paralyze))
    (fire-salts
        (drain-health fortify-agility resist-frost fire-shield))
    (frost-salts
        (drain-speed restore-magicka frost-shield resist-fire))
    (ghoul-heart
        (paralyze cure-poison fortify-attack))
    (gold-kanet
        (drain-health burden drain-luck restore-strength))
    (gravedust
        (drain-intelligence cure-common-disease drain-magicka restore-endurance))
    (green-lichen
        (fortify-personality cure-common-disease drain-strength drain-health))
    (gaur-hide
        (drain-fatigue fortify-endurance restore-personality fortify-luck))
    (hackle-lo-leaf
        (restore-fatigue paralyze water-breathing restore-luck))
    (heather
        (restore-personality feather drain-speed drain-personality))
    (hound-meat
        (restore-fatigue fortify-fatigue reflect detect-enchantment))
    (hypha-facia
        (drain-luck drain-agility drain-fatigue detect-enchantment))))

    ;; to be continued




(define effectso (lambda (ingredient effects)
    (lookupo ingredient effects ingredient-table)))


(define lookupo (lambda (k v l)
    (fresh (k-l v-l cdr-l)
        (== l `((,k-l ,v-l) . ,cdr-l))
        (conde
            ((== k k-l) (== v v-l))
            ((lookupo k v cdr-l))))))

(define appendo (lambda (l r o)
    (conde
        ((== l '()) (== r o))
        ((fresh (car-l cdr-l cdr-o)
            (== l `(,car-l . ,cdr-l))
            (== o `(,car-l . ,cdr-o))
            (appendo cdr-l r cdr-o))))))

(define all-effectso (lambda (ingredients effects)
    (conde
        ((== ingredients '()) (== effects '()))

        ((fresh (car-ingredients cdr-ingredients init-effects rest-effects)
            (== ingredients `(,car-ingredients . ,cdr-ingredients))
            (appendo init-effects rest-effects effects)
            (effectso car-ingredients init-effects)
            (all-effectso cdr-ingredients rest-effects))))))



(define contains-twiceo (lambda (x l b)
    (conde
        ((== l '()) (== b #f))
        ((fresh (y) (== l `(,y)) (== b #f)))
        ((fresh (car-l cdr-l cadr-l cddr-l)
            (== l `(,car-l . cdr-l))
            (== cdr-l `(,cadr-l . ,cddr-l))
            (conde
                ((== x car-l) (containso x cdr-l b))
                ((=/= x car-l) (contains-twiceo x cdr-l b))))))))


(define containso (lambda (x l b)
    (conde
        ((== l '()) (== b #f))
        ((fresh (car-l cdr-l)
            (== l `(,car-l . ,cdr-l))
            (conde
                ((== x car-l) (== b #t))
                ((=/= x car-l) (containso x cdr-l b))))))))


(define filtero (lambda (l o)
    (conde
        ((== o '()))
        ((fresh (car-o cdr-o)
            (contains-twiceo car-o l #t)
            (filtero l cdr-o))))))



(define make-potiono (lambda (ingredients effects)
    (fresh (all-effects)
        (all-effectso ingredients all-effects)
        (filtero all-effects effects))))

