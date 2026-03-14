/**
 * @callback RenderFunction
 * @param {Engine} engine
 * @param {Context} context
 */

function todo() {
    throw new Error("Not implemented")
}

const None = Symbol.for("None")
const True = true
const False = false

const VarNotFound = ""

class Context {
    #entries = new Map()
    /** @type {Map<string,any>|Context}*/
    #parentContext = new Map(Object.entries({None, True, False}))

    constructor(dict, parentContext = undefined) {
        this.#entries = new Map(Object.entries(dict))
        if (parentContext instanceof Context) {
            this.#parentContext = parentContext
        }
    }

    extend(dict) {
        return new Context(dict, this)
    }

    has(key) {
        return this.#entries.has(key) || this.#parentContext.has(key)
    }

    get(key) {
        if (this.#entries.has(key)) {
            return this.#entries.get(key)
        }
        if (this.#parentContext.has(key)) {
            return this.#parentContext.get(key)
        }
        return VarNotFound
    }
}


class SafeString {
    constructor(value) {
        this.value = value
    }

    static isSafe(value) {
        return value instanceof SafeString
    }

    [Symbol.toStringTag]() {
        if (this.value.hasOwnProperty("__html__")) {
            return this.value.__html__()
        } else if (typeof value === "function") {
            return this.value()
        } else {
            return String(this.value)
        }
    }
}

function markSafe(value) {
    return new SafeString(value)
}

function escape(value) {
    return SafeString.isSafe(value) ? value : new Option(String(value)).innerHTML
}

function toInteger(value) {
    const result = parseInt(value, 10)
    if (!Number.isFinite(result)) {
        throw SyntaxError(`${value} is not a integer`)
    }
    return result
}

class Filters {
    constructor(engine) {
        this.engine = engine
        return new Proxy(Object.freeze(this), {
            get(target, prop) {
                const result = target[prop]
                if (result === undefined) {
                    throw new Error(`Unknown filter: ${prop}`)
                }
                return result
            }
        })
    }

    add(value, arg) {
        const a = (typeof value === "number") ? value : parseFloat(String(value))
        const b = (typeof arg === "number") ? arg : parseFloat(String(value))
        if (Number.isNaN(a) || Number.isNaN(b)) {
            return ""
        }
        return String(a + b)
    }

    addslashes(value) {
        return String(value).replace(/\\/, "\\\\").replace('"', '\\"').replace("'", "\\'")
    }

    capfirst(value) {
        value = escape(value)
        if (value === "") {
            return ""
        } else if (value.length === 1) {
            return value.charAt(0).toUpperCase()
        }
        return `${value.charAt(0).toUpperCase()}${value.substring(1)}`
    }

    center(value, arg) {
        arg = toInteger(arg)
        value = escape(arg)
        if (arg < 0 || arg < value.length) {
            return value
        }
        const left = Math.floor((arg - value.length) / 2)
        const right = Math.ceil((arg - value.length) / 2)
        return value.padStart(left, " ").padEnd(right, " ")
    }

    cut(value, arg) {
        const safe = SafeString.isSafe(value)
        const result = String(value).replace(arg, "")
        if (safe && arg !== ";") {
            return markSafe(result)
        }
        return escape(result)
    }

    date() {
        todo()
    }

    default(value, arg) {
        return value === "" ? arg : value
    }

    default_if_none(value, arg) {
        return value === None ? arg : value
    }

    dictsort() {
        todo()
    }

    dictsortreversed() {
        todo()
    }

    divisibleby(value, arg) {
        if (!Number.isFinite(value) && typeof value !== "bigint") {
            throw SyntaxError(`${value} is neither a Number of BigInt`)
        }
        if (typeof value === "bigint") {
            return value % BigInt(arg)
        }
        return value % parseInt(arg, 10)
    }

    escape(value) {
        return escape(value)
    }

    escapejs() {
        todo()
    }

    escapeseq() {
        todo()
    }

    filesizeformat() {
        todo()
    }

    first() {
        todo()
    }

    floatformat() {
        todo()
    }

    force_escape() {
        todo()
    }

    get_digit() {
        todo()
    }

    iriencode() {
        todo()
    }

    join() {
        todo()
    }

    json_script() {
        todo()
    }

    last() {
        todo()
    }

    length() {
        todo()
    }

    linebreaks() {
        todo()
    }

    linebreaksbr() {
        todo()
    }

    linenumbers() {
        todo()
    }

    ljust() {
        todo()
    }

    lower(arg) {
        return String(arg).toLocaleLowerCase()
    }

    make_list() {
        todo()
    }

    phone2numeric() {
        todo()
    }

    pluralize() {
        todo()
    }

    pprint() {
        todo()
    }

    random() {
        todo()
    }

    rjust() {
        todo()
    }

    safe() {
        todo()
    }

    safeseq() {
        todo()
    }

    slice() {
        todo()
    }

    slugify() {
        todo()
    }

    stringformat() {
        todo()
    }

    striptags() {
        todo()
    }

    time() {
        todo()
    }

    timesince() {
        todo()
    }

    timeuntil() {
        todo()
    }

    title() {
        todo()
    }

    truncatechars() {
        todo()
    }

    truncatechars_html() {
        todo()
    }

    truncatewords() {
        todo()
    }

    truncatewords_html() {
        todo()
    }

    unordered_list() {
        todo()
    }

    upper() {
        todo()
    }

    urlencode() {
        todo()
    }

    urlize() {
        todo()
    }

    urlizetrunc() {
        todo()
    }

    wordcount() {
        todo()
    }

    wordwrap() {
        todo()
    }

    yesno() {
        todo()
    }
}


class Tags {
    #engine

    constructor(engine) {
        this.#engine = engine

        return new Proxy(Object.freeze(this), {
            get(target, prop, receiver) {
                if (Reflect.has(target, prop)) {
                    return Reflect.get(target, prop, receiver)
                }
                if (Reflect.has(target, `do_${prop}`)) {
                    return Reflect.get(target, `do_${prop}`, receiver).bind(target)
                }
                throw new Error(`Unknown tag: ${prop}`)
            }
        })
    }

    /**
     *
     * @param params
     * @param {Context} params.context
     * @param {Iterable} params.iter
     * @param {function(any): Object<string, any>} params.forloopVariables
     * @param {boolean} params.reversed
     * @param {RenderFunction} params.render
     * @param {RenderFunction | null} params.emptyRender
     * @returns {string}
     */
    do_for({context, iter, forloopVariables, render, emptyRender = null, reversed = false}) {
        const parentloop = context.get("forloop") || None
        const container = Array.from(iter)

        function getContext(counter, value) {
            return context.extend({
                ...forloopVariables(value),
                forloop: {
                    get counter() {
                        return counter + 1
                    },
                    get counter0() {
                        return counter
                    },
                    get revcounter() {
                        return container.length - counter
                    },
                    get revcounter0() {
                        return container.length - counter - 1
                    },
                    get first() {
                        return counter === 0
                    },
                    get last() {
                        return counter === container.length - 1
                    },
                    get length() {
                        return container.length
                    },
                    parentloop,
                }
            })
        }

        if (container.length === 0) {
            return emptyRender?.call(null, this.#engine, context) ?? VarNotFound
        }
        const reducer = reversed ? container.reduceRight.bind(container) : container.reduce.bind(container)
        return reducer((acc, value, idx) => acc + render(this.#engine, getContext(idx, value)), "")
    }
}

const utils = Object.freeze({
    contains(first, second) {
        return Array.from(first).contains(second)
    },
    do_is(first, second) {
        return first === second
    },
})

class Engine {
    #tags = new Tags(this)
    #filters = new Filters(this)

    constructor() {
        this._ = utils
    }

    /**
     * @param {Object} dict
     * @returns {Context}
     */
    context(dict) {
        return new Context(dict)
    }

    /**
     * @param {string[]|undefined} varNames
     * @param {any|undefined} literal
     * @param {Context} context
     * @param {string[]} filters
     * @returns {any}
     */
    variable({varNames = [], literal = undefined, context, filters}) {
        if (literal !== undefined) {
            return `${literal}`
        }

        let target = context
        for (let varName of varNames) {
            const orig = target
            if (target instanceof Context || target instanceof Map) {
                target = target.get(varName)
            } else if ((target instanceof Set && target.has(varName)) || Object.hasOwn(target, varName)) {
                target = target[varName]
            } else {
                target = VarNotFound
            }

            if (typeof target === "function") {
                target = target.call(orig)
            } else if (target === null || target === undefined) {
                target = VarNotFound
            }
        }

        for (const {filterName, argument} of filters) {
            target = this.#filters[filterName](target, argument)
        }

        return target
    }

    escape(value) {
        return escape(`${value}`)
    }

    tag({tagName, args, context}) {
        return this.#tags[tagName]({...args, context})
    }

    translate(value) {
        console.warn("Translation not implemented")
        return value
    }


}

export default Engine
export {Engine, markSafe, escape, None, True, False}
