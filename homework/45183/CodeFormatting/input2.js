export default class Queue {
    constructor() {
        this._firstNode = null;
        this._lastNode = null;
    }

    /**
     * Retrieves the first element in the queue.
     * It also removes it from the queue.
     * @returns {*}
     */
    dequeue() {
        if (!this._firstNode) {
            return this._firstNode;
        }

        const payload = this._firstNode.payload;

        this._firstNode = this._firstNode.next;

        return payload;
    }

    /**
     * Adds an item to the queue
     * @param {*} item
     */
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

    /**
     * Empties the queue
     */
    clear() {
        this._firstNode = null;
        this._lastNode = null;
    }

    /**
     * Gets the first element of the queue
     * @returns {*}
     */
    top() {
        if (!this._firstNode) {
            return this._firstNode;
        }

        return this._firstNode.payload;
    }

    /**
     * Checks whether the queue is empty or not
     * @returns {boolean}
     */
    isEmpty() {
        return !this._firstNode;
    }
}