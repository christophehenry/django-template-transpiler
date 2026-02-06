function todo() {
    throw new Error("Not implemented")
}

const None = Symbol.for("None")
const True = true
const False = false


class Context {
    constructor(dict, parentContext = undefined) {
        this._entries = new Map(Object.entries(dict))

        this.parentContext = (
            parentContext instanceof Context
                ? parentContext
                : new Proxy(new Map(Object.entries({None, True, False})), {
                    get(target, prop) {
                        const value = target.get(prop)
                        return value !== undefined ? value : ""
                    }
                })
        )

        return new Proxy(this, {
            get(target, prop) {
                const value = target._entries.get(prop)
                return value !== undefined ? value : target.parentContext[prop]
            }
        })
    }

    extend(dict) {
        return new Context(dict, this)
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

function toNumber(value) {
    return Number.isFinite(value) ? value : parseFloat(String(value))
}

function toInteger(value) {
    const result = parseInt(value, 10)
    if (!Number.isFinite(result)) {
        throw SyntaxError(`${value} is not a integer`)
    }
    return result
}

class Filters {
    constructor() {
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

const utils = Object.freeze({
    contains(first, second) {
        return Array.from(first).contains(second)
    },
    do_is(first, second) {
        return first === second
    }
})

class Engine {
    constructor() {
        this._filters = new Filters()
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
     * @param {string} varName
     * @param {any} literal
     * @param {Context} context
     * @param {Filters} filters
     * @returns {string}
     */
    variable({varName, literal, context, filters}) {
        let value = literal || context[varName]
        for (const {filterName, argument} of filters) {
            value = escape(this._filters[filterName](value, argument))
        }
        return value
    }

    translate(value) {
        console.warn("Translation not implemented")
        return value
    }

}

export default Engine
export {Engine, markSafe, escape, None, True, False}
