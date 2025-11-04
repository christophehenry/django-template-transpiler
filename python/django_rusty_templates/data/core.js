function todo() {
    throw new Error("Not implemented")
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

    add(value) {
        todo()
    }

    addslashes(value) {
        todo()
    }

    capfirst(value) {
        todo()
    }
}


class Engine {
    constructor() {
        this._filters = new Filters()
    }

    _escape(content) {
        return new Option(content).innerHTML
    }

    _filter(name, argument) {
        debugger
        const filter = this._filters[name]
        if (filter === undefined) {
            throw new Error(`Unknown filter: ${name}`)
        }
        return argument === undefined ? value => filter(value, argument) : filter;
    }

    variable({context, name, filters}) {
        let result = context.hasOwnProperty(name) ? this._escape(context[name]) : ""
        for (const {name, argument} of filters) {
            result = this._filter(name, argument)(result)
        }
        return result
    }
}

export default Engine
export {Engine}
