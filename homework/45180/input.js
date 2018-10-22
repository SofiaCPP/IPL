export default class Queue {
    constructor() {
        this._firstNode = null;
        this._lastNode = null;
    }

    dequeue() {
        if (!this._firstNode) {
            return this._firstNode;
        }

        const payload = this._firstNode.payload;

        this._firstNode = this._firstNode.next;

        return payload;
    }

    enqueue(item) {
        const newNode = {
            payload: item,
            next: null
        };

        if (this._firstNode === null) {
            this._firstNode = newNode;
            this._lastNode = newNode;

        } else {
            this._lastNode.next = newNode;
            this._lastNode = newNode;
        }
    }

    clear() {
        this._firstNode = null;
        this._lastNode = null;
    }

    top() {
        if (!this._firstNode) {
            return this._firstNode;
        }

        return this._firstNode.payload;
    }

    isEmpty() {
        return !this._firstNode;
    }
}
