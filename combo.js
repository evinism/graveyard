const letters = 'abcdefghijlmnopqrstuvwzyz';
const nums = '1234567890';
const specs = '~!@#$%^&*()_+{}[]-=\\|/?<>.,`"\''
X = (letters + letters.toUpperCase() + nums + specs).split('');

class GreedyOver {
    constructor(parsers, name){
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

        return tokens;
    }
}

class CharAcceptor {
    constructor(alph, name){
        this.alph = alph;
        this.name = name;
    }
    
    splitAlph(firstAlph, firstName, secondName){
        const secondAlph = 
            this.alph.filter(char => !firstAlph.includes(char));
        const parser1 = new CharAcceptor(firstAlph, firstName);
        const parser2 = new CharAcceptor(secondAlph, secondName);
        return new GreedyOver([parser1, parser2]);
    }

    parseStr(str){
        for (let char of str) {
            if(!this.alph.includes(char)){
                return {
                    type: 'error',
                }
            }
        }
        return {
            type: 'token',
            name: this.name,
            data: str
        };
    }
}


const ca = new CharAcceptor(X, 'whole alphabet').splitAlph('aeiou', 'vowels', 'letters');

console.log(ca.parseStr('hsezalloae'));
