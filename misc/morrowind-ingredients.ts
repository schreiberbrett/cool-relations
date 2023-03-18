const ingredientEffects: Map<string, string[]> = new Map([
    ['alit-hide', ['drain-intelligence', 'resist-poision', 'telekinesis', 'detect-animal']],
    ['ampoule-pod', ['water-walking', 'paralyze', 'detect-animal', 'drain-willpower']],
    ['ash-salts', ['drain-agility', 'resist-magicka', 'cure-blight-disease']],
    ['ash-yam', ['fortify-intelligence', 'fortify-strength', 'resist-common-disease', 'detect-key']],
    ['bittergreen-petals', ['restore-intelligence', 'invisibility']],
    ['black-anther', ['drain-agility', 'resist-fire', 'drain-endurance', 'light']],
    ['black-lichen', ['drain-strength', 'resist-frost', 'drain-speed', 'cure-poison']],
    ['bloat', ['drain-magicka', 'fortify-intelligence', 'fortify-willpower', 'detect-animal']],
    ['bonemeal', ['restore-agility', 'telekinesis', 'drain-fatigue', 'drain-personality']],
    ['bread', ['restore-fatigue']],
    ['bunglers-bane', ['drain-speed', 'drain-endurance', 'dispel', 'drain-strength']],
    ['chokeweed', ['drain-luck', 'restore-fatigue', 'cure-common-disease', 'drain-willpower']],
    ['coda-flower', ['drain-personality', 'levitate', 'drain-intelligence', 'drain-health']],
    ['comberry', ['drain-fatigue', 'restore-magicka', 'fire-shield', 'reflect']],
    ['corkbulb-root', ['cure-paralyzation', 'restore-health', 'lightning-shield', 'fortify-luck']],
    ['corprus-weepings', ['drain-fatigue', 'fortify-luck', 'drain-willpower', 'restore-health']],
    ['crab-meat', ['restore-fatigue', 'resist-shock', 'lightning-shield', 'restore-luck']],
    ['daedra-skin', ['fortify-strength', 'cure-common-disease', 'paralyze', 'swift-swim']],
    ['daedras-heart', ['restore-magicka', 'fortify-endurance', 'drain-agility', 'night-eye']],
    ['diamond', ['drain-agility', 'invisibility', 'reflect', 'detect-key']],
    ['dreugh-wax', ['fortify-strength', 'restore-strength', 'drain-luck', 'drain-willpower']],
    ['ectoplasm', ['fortify-agility', 'detect-animal', 'drain-strength', 'drain-health']],
    ['emerald', ['fortify-magicka', 'restore-health', 'drain-agility', 'drain-endurance']],
    ['fire-petal', ['resist-fire', 'drain-health', 'spell-absorption', 'paralyze']],
    ['fire-salts', ['drain-health', 'fortify-agility', 'resist-frost', 'fire-shield']],
    ['frost-salts', ['drain-speed', 'restore--magicka', 'frost-shield', 'resist-fire']],
    ['ghoul-heart', ['paralyze', 'cure-poison', 'fortify-attack']],
    ['gold-kanet', ['drain-health', 'burden', 'drain-luck', 'restore-strength']],
    ['gravedust', ['drain-intelligence', 'cure-common-disease', 'drain-magicka', 'restore-endurance']],
    ['green-lichen', ['fortify-personality', 'cure-common-disease', 'drain-strength', 'drain-health']],
    ['gaur-hide', ['drain-fatigue', 'fortify-endurance', 'restore-personality', 'fortify-luck']],
    ['hackle-lo-leaf', ['restore-fatigue', 'paralyze', 'water-breathing', 'restore-luck']],
    ['heather', ['restore-personality', 'feather', 'drain-speed', 'drain-personality']],
    ['hound-meat', ['restore-fatigue', 'fortify-fatigue', 'reflect', 'detect-enchantment']],
    ['hypha-facia', ['drain-luck', 'drain-agility', 'drain-fatigue', 'detect-enchantment']],
    ['kagouti-hide', ['drain-fatigue', 'fortify-speed', 'resist-common-disease', 'night-eye']],
    ['kresh-fiber', ['restore-luck', 'fortify-personality', 'drain-magicka', 'drain-speed']],
    ['kwama-cuttle', ['resist-poison', 'drain-fatigue', 'water-walking', 'water-breathing']],
    ['large-kwama-egg', ['restore-fatigue', 'paralyze', 'frost-shield', 'fortify-health']],
    ['luminous-russula', ['water-breathing', 'drain-fatigue', 'poison']],
    ['marshmerrow', ['restore-health', 'detect-enchantment', 'drain-willpower', 'drain-fatigue']],
    ['moon-sugar', ['fortify-speed', 'dispel', 'drain-endurance', 'drain-luck']],
    ['muck', ['drain-intelligence', 'detect-key', 'drain-personality', 'cure-common-disease']],
    ['netch-leather', ['fortify-endurance', 'fortify-intelligence', 'drain-personality', 'cure-paralyzation']],
    ['pearl', ['drain-agility', 'dispel', 'water-breathing', 'resist-common-disease']],
    ['racer-plumes', ['drain-willpower', 'levitate']],
    ['rat-meat', ['drain-magicka', 'paralyze', 'cure-poison', 'resist-poison']],
    ['raw-ebony', ['drain-agility', 'cure-poison', 'frost-shield', 'restore-speed']],
    ['raw-glass', ['drain-intelligence', 'drain-strength', 'drain-speed', 'fire-shield']],
    ['red-lichen', ['drain-speed', 'light', 'cure-common-disease', 'drain-magicka']],
    ['resin', ['restore-health', 'restore-speed', 'burden', 'resist-common-disease']],
    ['roobrush', ['drain-willpower', 'fortify-agility', 'drain-health', 'cure-poison']],
    ['ruby', ['drain-health', 'feather', 'restore-intelligence', 'drain-agility']],
    ['saltrice', ['restore-fatigue', 'fortify-magicka', 'drain-strength', 'restore-health']],
    ['scales', ['drain-personality', 'water-walking', 'restore-endurance', 'swift-swim']],
    ['scamp-skin', ['drain-magicka', 'cure-paralyzation', 'restore-personality', 'restore-strength']],
    ['scathecraw', ['drain-strength', 'cure-poison', 'drain-health', 'restore-willpower']],
    ['scrap-metal', ['drain-health', 'lightning-shield', 'resist-shock', 'restore-intelligence']],
    ['scrib-jelly', ['fortify-willpower', 'cure-poison', 'cure-blight-disease', 'restore-willpower']],
    ['scrib-jerky', ['restore-fatigue', 'fortify-fatigue', 'burden', 'swift-swim']],
    ['scuttle', ['restore-fatigue', 'fortify-fatigue', 'feather', 'telekinesis']],
    ['shalk-resin', ['drain-fatigue', 'fortify-health', 'drain-personality', 'fortify-speed']],
    ['sload-soap', ['drain-personality', 'fortify-agility', 'fire-shield', 'restore-agility']],
    ['small-kwama-egg', ['restore-fatigue']],
    ['spore-pod', ['drain-strength', 'drain-fatigue', 'detect-key', 'paralyze']],
    ['stoneflower-petals', ['restore-strength', 'fortify-magicka', 'drain-luck', 'fortify-personality']],
    ['trama-root', ['restore-willpower', 'levitate', 'drain-magicka', 'drain-speed']],
    ['vampire-dust', ['fortify-heatlh', 'fortify-strength', 'spell-absorption', 'vampirism']],
    ['violet-coprinus', ['water-walking', 'drain-fatigue', 'poison']],
    ['void-salts', ['restore-magicka', 'spell-absorption', 'paralyze', 'drain-endurance']],
    ['wickwheat', ['restore-health', 'fortify-willpower', 'paralyze', 'damage-intelligence']],
    ['willow-anther', ['drain-personality', 'frost-shield', 'cure-common-disease', 'cure-paralyzation']]
])

const allIngredients = [...ingredientEffects.keys()];

function isSubset<T>(subset: Set<T>, superset: Set<T>): boolean {
    for (const x of subset) {
        if (!superset.has(x)) {
            return false;
        }
    }

    return true;
}

function make(ingredients: Set<string>): Set<string> {
    let effectCount = new Map<string, number>();

    for (const ingredient of ingredients) {
        const effects = ingredientEffects.get(ingredient)
        if (effects) {
            for (const effect of effects) {
                let n = effectCount.get(effect) ?? 0;
                effectCount.set(effect, n + 1);
            }
        }
    }

    let result = new Set<string>();
    for (const [effect, count] of effectCount.entries()) {
        if (count > 1) {
            result.add(effect);
        }
    }

    return result;
}

function solve(desiredEffects: Set<string>): {ingredients: Set<string>, effects: Set<string>}[] {
    let result = new Array<{ingredients: Set<string>, effects: Set<string>}>();

    for (let i = 0; i < allIngredients.length; i++) {
        for (let j = 0; j < i; j++) {
            const ingredients = new Set([allIngredients[i], allIngredients[j]])

            const effects = make(ingredients);

            if (isSubset(desiredEffects, effects)) {
                result.push({ingredients, effects});
            }
        }
    }

    if (result.length > 0) {
        return result;
    }

    for (let i = 0; i < allIngredients.length; i++) {
        for (let j = 0; j < i; j++) {
            for (let k = 0; k < j; k++) {
                const ingredients = new Set([allIngredients[i], allIngredients[j], allIngredients[k]])

                const effects = make(ingredients);

                if (isSubset(desiredEffects, effects)) {
                    result.push({ingredients, effects});
                }
            }
        }
    }

    if (result.length > 0) {
        return result;
    }

    for (let i = 0; i < allIngredients.length; i++) {
        for (let j = 0; j < i; j++) {
            for (let k = 0; k < j; k++) {
                for (let l = 0; l < k; l++) {
                    const ingredients = new Set([allIngredients[i], allIngredients[j], allIngredients[k], allIngredients[l]])

                    const effects = make(ingredients);

                    if (isSubset(desiredEffects, effects)) {
                        result.push({ingredients, effects});
                    }
                }
            }
        }
    }

    return result;
}

// @ts-ignore
console.log(solve(new Set(Deno.args)))