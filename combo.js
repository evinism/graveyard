const letters = 'abcdefghijlmnopqrstuvwzyz';
const nums = '1234567890';
const specs = '~!@#$%^&*()_+{}[]-=\\|/?<>.,`"\''
X = (letters + letters.toUpperCase() + nums + specs).split('');

class InvParser {
    constructor(alph){
        this.alph = alph;
    }

    splitAlph(firstAlph, firstName, secondName){
        const secondAlph = 
            this.alph.filter(char => !firstAlph.includes(char));
        const parser1 = new CharAcceptor(firstAlph, this, firstName);
        const parser2 = new CharAcceptor(secondAlph, this, secondName);
        return new GreedyOver(this.alph, [parser1, parser2]);
    }

}

class GreedyOver extends InvParser {
    constructor(alph, parsers, name){
        super(alph);
        this.parsers = parsers;
        this.name = name;
    }

    getFirstToParse(str){
        for(let parser of this.parsers) {
            let potentialToken = parser.parseStr(str);
            if (potentialToken.type !== 'error') {
                return potentialToken;
            }
        }
        return {
            type: 'error'
        }
    }

    parseStr(str){
        const tokens = [];
        let start = 0;
        let end = 1;
        do {
            let curSubstr = str.slice(start, end);
            let nextSubstr = str.slice(start, end + 1);
            if (end === str.length || this.getFirstToParse(nextSubstr).type === 'error') {
                tokens.push(this.getFirstToParse(curSubstr));
                start = end;
            } else {
                end++;
            }
        } while (start < str.length);

        return {
            type: 'sequence',
            tokens
        };
    }
}

class Tokenizer extends InvParser {
    constructor(alph, name){
        super(alph);
        this.name = name;
    }
    
    parseStr(str){
        return {
            type: 'token',
            name: this.name,
            data: str,
        }
    }
}

class CharAcceptor extends InvParser {
    constructor(alph, supportParser, name){
        super(alph);
        this.name = name;
        this.supportParser = supportParser;
    }

    parseStr(str){
        for (let char of str) {
            if(!this.alph.includes(char)){
                return {
                    type: 'error',
                }
            }
        }
        return this.supportParser.parseStr(str);
    }
}

const flattenSequences = sequenceToken => {
    if (sequenceToken.type !== 'sequence')
        throw "lol tried to flatten non sequence";
    sequenceToken.tokens.forEach(token => {
        if(token.type !== 'sequence'){
            throw "lol inner of flatten sequence is not sequence"
        }
    });
    return {
        type: 'sequence',
        tokens: sequenceToken
            .tokens
            .map(token => token.tokens)
            .reduce((acc, cur) => acc.concat(cur), [])
    }
}

const ca = new CharAcceptor(X, new Tokenizer(X), 'whole alphabet').splitAlph('a', 'letters').splitAlph('aeiou', 'vowels');

console.log(JSON.stringify(flattenSequences(ca.parseStr('hsezallooeaoo'))));
