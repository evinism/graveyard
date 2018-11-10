const rawApi = (function(){
    uuidStore = [];
    const staticNetDelay = 500
    const dynamicNetDelay = 10;
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

    const postUUID = msg => new Promise((resolve, reject) => {
        netDelay(() => {
            const result = persistUUID(msg).success
                ? resolve
                : reject;
            console.log('memory stuff: ' + uuidStore.map(a => a ? 1 : 0))
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
    hitSeries(start, data){
        const addresses = data
            .map((val, i) => val * (i + start))
            .filter((_, i) => data[i])
        console.log('will hit:', addresses);
        const requests = addresses.map(address => this.hitAddr(address));
        
        return Promise.all(requests)
    }

    hitMemRange(start, end){
        return this.hitSeries(start, Array(end - start).fill(1))
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



class Client {
    constructor(){
        //init();
    }

    async init(){
        const result = await api.hitMemRange(0, w(1))
        if (!result.every(Boolean)) {
            // We got the lock and are now obligated to 
            // instantiate the HeadPtr
            console.log('creation lock acquired');
            await this.writeHeadPtr(w(1), 15);
        }
        // One second intervals
        this.inSequence = false;
        setInterval(async () => {
            if(this.inSequence === false){
                console.log('=== BEGINNING NEW SEQUENCE ===')
                this.inSequence = true;
                await this.readHeadPtr();
                this.inSequence = false;
            }
        }, 2000);
    }

    async readHeadPtr(){
        let lastRead;
        let currentWord = 0;
        // forward scan, pulling off head ptr
        do {
            currentWord++;
            lastRead = await api.hitMemRange(
                w(currentWord),
                w(currentWord + 1)
            );
        } while(lastRead.every(Boolean));

        if(lastRead[1]) {
            // We got a read + lock
            const value = parseInt(lastRead.slice(2).join(''), 2)

            console.log('lock acquired w/ a ' + value)
            await this.writeHeadPtr(
                w(currentWord + 1),
                value
            );
        } else {
            console.log('lock rejected');
        }
    }

    writeHeadPtr(address, headPtrValue){
        const inBinary = headPtrValue
            .toString(2)
            .slice(-ptrLength)
            .padStart(ptrLength, '0')
            .split('')
            .map(digit => parseInt(digit));
        return api.hitSeries(address, [].concat([0, 1], inBinary))
    }
}


client1 = new Client();

client1.init();