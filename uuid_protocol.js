
// Stupid utilites
let uuidStore, requestCounter;

const testRate = () => {
    requestCounter = 0;
    this.setTimeout(
        () => console.log('request rate: ' + (requestCounter / 10) + ' per second'),
        10000
    );
}

var audioCtx = new (window.AudioContext || window.webkitAudioContext || window.audioContext);

//All arguments are optional:

//duration of the tone in milliseconds. Default is 500
//frequency of the tone in hertz. default is 440
//volume of the tone. Default is 1, off is 0.
//type of tone. Possible values are sine, square, sawtooth, triangle, and custom. Default is sine.
//callback to use on end of tone
function beep(duration, frequency, volume, type, callback) {
    var oscillator = audioCtx.createOscillator();
    var gainNode = audioCtx.createGain();

    oscillator.connect(gainNode);
    gainNode.connect(audioCtx.destination);

    if (volume){gainNode.gain.value = volume;};
    if (frequency){oscillator.frequency.value = frequency;}
    if (type){oscillator.type = type;}
    if (callback){oscillator.onended = callback;}

    oscillator.start();
    setTimeout(function(){oscillator.stop()}, (duration ? duration : 500));
};

// Api representation:
const rawApi = (function(){
    uuidStore = [];
    const staticNetDelay = 200
    const dynamicNetDelay = 5; // It does NOT handle jitter well.
    const persistUUID = ({uuid, data}) => {
        if (!uuidStore[uuid]) {
            uuidStore[uuid] = data;
            return {
                success: true,
            }
        } else {
            return {
                success: false,
            }
        }
    }

    const netDelay = fn => setTimeout(
        fn,
        Math.random() * dynamicNetDelay + staticNetDelay
    );

    function debounce(func, wait, immediate) {
        var timeout;
        return function() {
            var context = this, args = arguments;
            var later = function() {
                timeout = null;
                if (!immediate) func.apply(context, args);
            };
            var callNow = immediate && !timeout;
            clearTimeout(timeout);
            timeout = setTimeout(later, wait);
            if (callNow) func.apply(context, args);
        };
    };

    const logMemContents = debounce(
        () => {
            let str = '';
            for(let i = 0; i < uuidStore.length; i++){
                str += uuidStore[i] ? '1' : '0';
            }
            //console.log('memory: ' + str)
        }, 
        10
    );

    requestCounter = 0;
    const postUUID = msg => new Promise((resolve, reject) => {
        netDelay(() => {
            // beep(10, 3000, 1, 'sine') // annoying but useful
            requestCounter++;
            const result = persistUUID(msg).success
                ? resolve
                : reject;
            logMemContents()
            netDelay(result);
        })
    });
    return { postUUID };
})();

class ApiHelper {
    constructor(server){
        this.server = server;
    }

    hitAddr(addr){
        return this.server.postUUID({
            uuid: addr,
            data: 'bogus',
        }).then(() => 0, () => 1);
    }

    // Where data is [0, 1, 0, 0, 1...]
    hitSeries(start, data, name=''){
        const addresses = data
            .map((val, i) => val * (i + start))
            .filter((_, i) => data[i])
        //console.log(name + ': will hit ', addresses);
        const requests = addresses.map(address => this.hitAddr(address));
        
        return Promise.all(requests)
    }

    hitMemRange(start, end, name=''){
        return this.hitSeries(start, Array(end - start).fill(1), name)
    }
}

const api = new ApiHelper(rawApi);

// config stuff
const memorySize = 32768; // for now
const ptrLength = Math.log2(memorySize);
// For now, word size is size of headPtrLength
const headPtrLength = 2 + ptrLength;
// w is word num -> address
const w = num => num * headPtrLength

// First byte is special-- writing first byte aqcuires both 
// a creation lock and a a rw lock. A creation lock means
// that 
// and tasks the client w/ setting up the structure. 


const delays = {
    pollInterval: 200,
    workTime: 500,
    postReleaseDelay: 500,
}

class Client {
    constructor(name){
        //init();
        this.name = name
    }

    async init(){
        const result = await api.hitMemRange(0, w(1), this.name)
        if (!result.every(Boolean)) {
            // We got the lock and are now obligated to 
            // instantiate the HeadPtr
            console.log(this.name + ': creation lock acquired');
            await this.writeHeadPtr(w(1), 15);
        }
        this.onesSledStartPoint = 0;

        // Two second intervals
        this.inSequence = false;
        setInterval(async () => {
            if(this.inSequence === false){
                //console.log(this.name + ': === BEGINNING NEW SEQUENCE ===')
                this.inSequence = true;
                await this.readHeadPtr();
                this.inSequence = false;
            }
        }, delays.pollInterval);
    }

    async readHeadPtr(){
        let lastRead;
        let currentWord = this.onesSledStartPoint;
        // forward scan, pulling off head ptr
        do {
            currentWord++;
            lastRead = await api.hitMemRange(
                w(currentWord),
                w(currentWord + 1),
                this.name
            );
        } while(lastRead.every(Boolean));

        this.onesSledStartPoint = currentWord;

        if(lastRead[1]) {
            // We got a read + lock
            const value = parseInt(lastRead.slice(2).join(''), 2)

            console.log(this.name + ': lock acquired w/ a ' + value)

            // do 5 seconds of work with the item
            await new Promise(res => setTimeout(res, delays.workTime))

            const nextValue = Math.floor(Math.random() * 200);
            await this.writeHeadPtr(
                w(currentWord + 1),
                nextValue
            );
            console.log(this.name + ': lock released w/ a ' + nextValue)
            await new Promise(res => setTimeout(res, delays.postReleaseDelay))
        } else {
            console.log(this.name + ': lock rejected');
        }
    }

    async writeHeadPtr(address, headPtrValue){
        const inBinary = headPtrValue
            .toString(2)
            .slice(-ptrLength)
            .padStart(ptrLength, '0')
            .split('')
            .map(digit => parseInt(digit));
        const results = await api.hitSeries(address, [].concat([0, 1], inBinary), this.name)
        if (results[0]) {
            // someone overwrote where we were going to get a head ptr.
            // try writes down the list until we get one.
            return this.writeHeadPtr(address + headPtrLength, headPtrValue);
        }
    }
}


client1 = new Client('client 1');
client2 = new Client('client 2');
client3 = new Client('client 3');
setTimeout(() => client1.init(), Math.random() * 10000);
setTimeout(() => client2.init(), Math.random() * 10000);
//setTimeout(() => client3.init(), Math.random() * 10000);
