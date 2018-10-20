//function greetings(name) {
//    return "Hello " + name + "!";
//}

//console.log(greetings("Ognyan"));

//12 += 3;

import Router from './Router';
import Utils from '../eventsApp/Common/Utils';
import GlobalSettings from '../settings/GlobalSettings';
import { createDynamicInput } from './eventHandlers/DynamicInputHandler';
import * as TabsHeaderScrollbar from './eventHandlers/TabsHeaderScrollbarHandler'
import { CLEvents } from './constants/CLEvents';
import { DocumentType } from './constants/DocumentType';
import UnitsTab from './UnitsTab/UnitsTab';
import OS from './OS';
import SymbolData from './SymbolData';
const utils = new Utils();
const regexExtPattern = /\.[0-9a-z]{1,5}$/i;

const fs = typeof cep_node !== 'undefined' ? cep_node.require('fs') : null;
const path = typeof cep_node !== 'undefined' ? cep_node.require('path') : null;

const CoherentUtilities = {
    // STRINGIFIABLE
    getMetadata: function () {
        var documentDom = fl.getDocumentDOM();
        var sceneScript = documentDom.getDataFromDocument('coherent_scene_script');
        var sceneProps = get_DocumentData();
        var defaultVertical = get_defaultVerticalUnits();
        var defaultHorizontal = get_defaultHorizontalUnits();
        var defaultPlayState = documentDom.getDataFromDocument('default_animation_play_state');

        var sceneObjectData = prepareSceneDataObject(sceneProps);

        var createMeta = function (customMetadata) {
            return '{"customMetadata":' + formatToObject(customMetadata.customMetadata) +
                ',"simpleLayoutScene":' + sceneObjectData.simpleLayout +
                ',"externalFiles":' + formatToArray(sceneObjectData.externalFiles) + '}';
        };
        var library = fl.getDocumentDOM().library;
        var timelines = documentDom.timelines;
        var scenesMeta = '{';

        for (var t = 0; t < timelines.length; t++) {
            var customMetadata = loopTimeline(timelines[t], library);
            scenesMeta += '\"' + timelines[t].name + '\":' + createMeta(customMetadata);
            if (t !== timelines.length - 1) {
                scenesMeta += ',';
            }
        }
        return scenesMeta + ',"sceneScript":' + sceneScript +
            ',"default_animation_play_state":"' + defaultPlayState + '"' +
            ',"default_units_type_vertical":"' + defaultVertical + '"' +
            ',"default_units_type_horizontal":"' + defaultHorizontal + '"' +
            '}';
    },

    loopScenes: function () {
        var documentDom = fl.getDocumentDOM();
        documentDom.selectNone();
        var timelines = documentDom.timelines;

        for (var t = 0; t < timelines.length; t++) {
            var res = { fn: prepareElementsForExport, param: [timelines[t]] };
            trampoline(res);
        }
    },

    prepareFlashElement: function (type, persistantMetadata) {
        fl.getDocumentDOM().convertToSymbol(type, "", "center");
        var element = cl_currentSelection[0].libraryItem;
        element.addData("coherent_meta", "string", persistantMetadata);
    },

    getJSPreferences: function () {
        if (!fl.getDocumentDOM()) {
            return "";
        }

        if (!fl.getDocumentDOM().getDataFromDocument('js-preferences')) {
            fl.getDocumentDOM().addDataToDocument("js-preferences", "string", "symbolInstance");
        }
        return fl.getDocumentDOM().getDataFromDocument('js-preferences');
    },

    /**
     * Basic GLOBAL PROPERTIES bootstrap
     * @function bootstrapGlobalProperties
     * @return {void}
     */
    bootstrapGlobalProperties: function () {
        evalScript('FLfile.exists(fl.configURI + "CoherentExtensions/global_settings.json")', (file) => {
            // TODO: Case not working!!
            if (file === 'false') {
                evalScript(`
                    if (!FLfile.exists(fl.configURI + 'CoherentExtensions/')) {
                        FLfile.createFolder(fl.configURI + 'CoherentExtensions/');
                    }
                `, () => {
                    this.bootstrapConfigSettings();
                });

            } else {
                evalScript('FLfile.read(fl.configURI + "CoherentExtensions/global_settings.json")', (data) => {
                    let props = JSON.parse(data);
                    let propsCoppy = Object.assign({}, props);
                    let propsLaterVersions = { playerParameters: {} };
                    if (!props.playerParameters["Coherent Exporter GT"]) {
                        propsLaterVersions.version = props.version;
                        propsLaterVersions.playerParameters["Coherent Exporter GT"] = Object.assign({}, props.playerParameters);
                        propsLaterVersions.playerParameters["Coherent Exporter HB"] = CONFIG_SETTINGS.playerParameters["Coherent Exporter HB"];
                    } else {
                        propsLaterVersions = props;
                    }
                    if (propsLaterVersions.version !== CONFIG_SETTINGS.version) {
                        this.bootstrapConfigSettings();
                    } else {
                        if (CURRENT_DOCUMENT_TYPE && CURRENT_DOCUMENT_TYPE !== "undefined") {
                            this.updateGTPlayer(JSON.stringify(propsLaterVersions));
                        }
                    }
                });
            }
        });
    },

    /**
     * Basic ANIMATE CONFIG BOOTSTRAP
     * @function bootstrapConfigSettings
     * @return {void}
     */
    bootstrapConfigSettings: function () {
        return new Promise((resolve, reject) => {
            evalScript('FLfile.write(fl.configURI + "CoherentExtensions/global_settings.json",\''
                + JSON.stringify(CONFIG_SETTINGS) + '\');', () => {
                    this.togglePlayButton();
                    resolve();
                });
        });
    },

    /**
     * Stringifier for converting methods to string
     * @function methodStringify
     * @param {void} method - method to stringify
     * @param {boolean} selfExecute - whether the method should
     * execute or just be placed for usage in the cep
     * @return {string} augmentedMethod - bootstrapped method
     */
    methodStringify: function (method, selfExecute) {
        var augmentedMethod = method.toString();
        var parameters = /\(\s*([^)]+?)\s*\)/.exec(augmentedMethod);
        if (parameters[1]) {
            parameters = parameters[1].split(/\s*,\s*/);
        }

        var parLength = parameters.length + 1;
        for (var i = 1; i < parLength; i++) {
            var injectedArgument = arguments[i + 1];
            var currentType = typeof injectedArgument;
            switch (currentType) {
                case "boolean":
                case "string":
                    var preparedVal = '"' + injectedArgument + '"';
                    augmentedMethod = augmentedMethod.replace(RegExp(parameters[i - 1], 'gm'),
                        preparedVal);
                    break;
                case "object":
                    var object = JSON.stringify(injectedArgument);
                    if (injectedArgument[0] === undefined) {
                        object = "'" + object + "'";
                    }
                    augmentedMethod = augmentedMethod.replace(RegExp(parameters[i - 1], 'gm'), object);
                    break;
                default:
                    break;
            }
        }

        var argumentsStr = augmentedMethod.match(/(?:function.*)(?=(\(\s*[^)]+?\s*\)))/, '()');
        if (argumentsStr) {
            augmentedMethod = augmentedMethod.replace(argumentsStr[1], '()');
        }

        if (selfExecute) {
            augmentedMethod = "(" + augmentedMethod + ")();"
        }

        return augmentedMethod;
    },

    unRegisterAllEventListeners: function (obj) {
        if (typeof obj._eventListeners == 'undefined' || obj._eventListeners.length == 0) {
            return;
        }

        for (var i = 0, len = obj._eventListeners.length; i < len; i++) {
            var e = obj._eventListeners[i];
            obj.removeEventListener(e.event, e.callback);
        }

        obj._eventListeners = [];
    },

    LoadCSSOrJS: function (filename, filetype) {
        if (filetype == "js") { //if filename is a external JavaScript file
            var fileref = document.createElement('script')
            fileref.setAttribute("type", "text/javascript")
            fileref.setAttribute("src", filename)
        }
        else if (filetype == "css") { //if filename is an external CSS file
            var fileref = document.createElement("link")
            fileref.setAttribute("rel", "stylesheet")
            fileref.setAttribute("type", "text/css")
            fileref.setAttribute("href", filename)
        }
        if (typeof fileref != "undefined")
            document.getElementsByTagName("head")[0].appendChild(fileref)
    },

    updateGTPlayer: function (data) {
        CONFIG_SETTINGS = JSON.parse(data);
        const keys = Object.keys(CONFIG_SETTINGS.playerParameters[CURRENT_DOCUMENT_TYPE] || {});
        this.togglePlayButton();

        for (var i = 0; i < keys.length; i++) {
            let currentKey = keys[i];
            if (currentKey === 'gtPlayerPath' || currentKey === 'hbPlayerPath') {
                document.getElementById(currentKey).value = CONFIG_SETTINGS.playerParameters[CURRENT_DOCUMENT_TYPE][currentKey];

            } else if (currentKey === 'renderer') {
                const value = CONFIG_SETTINGS.playerParameters[CURRENT_DOCUMENT_TYPE][currentKey];
                const selectInput = document.getElementById('gt-rend').__CLCustomControl__;

                selectInput.setValue(value);
            } else {
                document.getElementById(currentKey).checked = CONFIG_SETTINGS.playerParameters[CURRENT_DOCUMENT_TYPE][currentKey];
            }
        }
    },

    togglePlayButton: function () {
        const button = document.getElementById("play");
        let currentPlayerPath = (CURRENT_DOCUMENT_TYPE === "Coherent Exporter GT") ? "gtPlayerPath" : "hbPlayerPath";

        csInterface.evalScript('get_DocumentData()', (data) => {
            if ('null' === data || !this.isJson(data)) {
                button.disabled = true;
                return;
            }

            const currentPlayerOptions = CONFIG_SETTINGS.playerParameters[CURRENT_DOCUMENT_TYPE];

            if (currentPlayerOptions && !currentPlayerOptions[currentPlayerPath] || OS.isMac() && CURRENT_DOCUMENT_TYPE === DocumentType.GT) {
                button.disabled = true;
            } else {
                button.disabled = false;
            }
        });
    },

    updateSceneProperties: function (prop, value) {
        return new Promise((resolve, reject) => {
            if (CONFIG_SETTINGS.playerParameters[CURRENT_DOCUMENT_TYPE][prop] !== undefined) {
                if (prop === "gtPlayerPath" || prop === "hbPlayerPath") {
                    value = value.replace(/\\/g, "\\\\");
                }
                CONFIG_SETTINGS.playerParameters[CURRENT_DOCUMENT_TYPE][prop] = value;

                this.bootstrapConfigSettings().then(resolve).catch(reject);
            } else {
                evalScript('get_DocumentData()', (data) => {
                    data = data === 'null' ? '{}' : data;
                    const settings = JSON.parse(data);
                    let newFileFolder = value;

                    if (typeof value === 'string') {
                        newFileFolder = value.replace(/\\/g, "\\\\")
                    }

                    settings[prop] = newFileFolder;

                    evalScript(`set_DocumentData('${JSON.stringify(settings)}')`, () => {
                        this.togglePlayButton();
                        resolve();
                    });
                });
            }
        });
    },

    getDataFromElement: function (persistantMetadata) {
        var document = fl.getDocumentDOM();

        if (!document || document.type.indexOf('Coherent') !== 0) {
            return 'wrongScene';
        }

        if (!cl_currentSelection || cl_currentSelection.length !== 1) {
            return 'toolbarFlush';
        }

        var element = cl_currentSelection[0];

        if (element.instanceType !== 'symbol') {
            return 'notAnInstance';
        }

        if (element.instanceType === 'symbol' && element.symbolType !== 'graphic' && element.name === '') {
            return 'unnamed';
        }

        return get_DataProperties();
    },

    populate_textfield: function (prop, val) {
        this.updateSceneProperties(prop, val);
        document.getElementById(prop).value = val;
    },

    onSelectionChange: function (element) {
        if (PUBLISH_RUNNING) {
            return false;
        }

        Utils.getCurrentSelectonName().then((name) => {
            CURRENT_SELECTION_NAME = name;
            Utils.fireCustomEvent(CLEvents.SELECTION_CHANGED);
        });

        ONSEL_FOCUS_CLICK = (element !== "focus-panel");
        if (!(element === "focus-panel") && $vex !== null) {
            $vex.close();
        }

        if (element !== undefined) {
            evalScript(this.methodStringify(this.getDataFromElement, true, COHERENT_META_MODEL), this.onSelectionCallback.bind(this));
        }
    },

    onDocumentChange: function () {
        // We dispatch a custom document change event,
        // because the current handler is called at different times (change, close etc.)
        // and we need a mechanism to notify all tabs.
        // The idea is to decouple the code from the CoherentUtilities GOD glass.
        document.dispatchEvent(new CustomEvent(CLEvents.DOCUMENT_CHANGE));

        this.togglePlayButton();
        document.getElementById("set-as-component").checked = false;
        document.getElementById("components-msg").style.display = "none";

        this.synchroniseDocumentType(false).then((documentType) => {
            if (documentType !== DocumentType.GT && documentType !== DocumentType.HB) {
                // CASE: OPENS AN EMPTY FILE THAT IS NOT A COHERENT SCENE;
                SCENE_SELECTED = false;
                this.synchroniseDocumentType('undefined').then(() => {
                    this.flushPropertiesPanel();
                });

                return false;
            }

            evalScript('if (typeof get_DocumentData === "function"){get_DocumentData()}', (data) => {
                if (data === '0') {
                    SCENE_SELECTED = true;

                    // CASE: OPENS AN EMPTY FILE THAT IS A COHERENT SCENE;
                    let props = null;

                    if (CURRENT_DOCUMENT_TYPE === DocumentType.GT) {
                        props = JSON.stringify(SCENE_PROPERTIES_GT);
                    } else if (CURRENT_DOCUMENT_TYPE === DocumentType.HB) {
                        props = JSON.stringify(SCENE_PROPERTIES_HB);
                    }

                    if (props !== null) {
                        this.updatePluginInputs(props);
                    } else {
                        this.showSceneConvert(true);
                    }
                } else {
                    // TODO: Consider document change
                    SCENE_SELECTED = true;
                    this.updatePluginInputs(data);
                }
            });

            // We should enforce data persistence here
            evalScript(`if(typeof get_DataProperties == 'function') { get_DataProperties(); }`, (data) => {
                let parsedData = null;

                try {
                    parsedData = JSON.parse(data);

                    // After we change a our current document
                    // We need to updated the select value in order
                    // for the panel to reflect the real value of the position type.
                    document.getElementById('select-position').__CLCustomControl__.setValue(parsedData.position, true);
                } catch (e) {
                    if (data !== 'null') {
                        throw new Error('Failed to parse persistent data! Error: ' + e);
                    }
                }
            })
        }).catch(() => {
            // Catching when no document is opened.
        });
    },

    /**
     * Updates the cached document type.
     *
     * @param {*} force
     * @returns {Promise<any>}
     */
    synchroniseDocumentType: function (force) {
        return new Promise((resolve, reject) => {
            if (force) {
                CURRENT_DOCUMENT_TYPE = force;
                resolve(CURRENT_DOCUMENT_TYPE);
            } else {
                const script = `
                var currentDOM = fl.getDocumentDOM();
                if (currentDOM) {
                    currentDOM.type;
                } else {
                    null;
                }
                `;

                evalScript(script, (type) => {
                    if (type === 'null') {
                        reject('No document currently opened!');
                        return false;
                    }

                    CURRENT_DOCUMENT_TYPE = type;
                    resolve(CURRENT_DOCUMENT_TYPE);
                });
            }
        });
    },

    /**
     * Updates plugin inputs from persistant properties of the scene
     * @function updatePluginInputs
     * @param {string} data - persistant properties in the scene
     * @return {void}
     */
    updatePluginInputs: function (data) {
        const settings = JSON.parse(data);

        // Used by necessary views to update their inputs
        document.dispatchEvent(new CustomEvent(CLEvents.INPUTS_UPDATE, {
            detail: settings
        }));
    },

    capitalizeFirstLetter: function (string) {
        const temp = string.toLowerCase();
        return temp.charAt(0).toUpperCase() + temp.slice(1);
    },

    addClass: function (elements, myClass) {
        // if there are no elements, we're done
        if (!elements) {
            return;
        }

        // if we have a selector, get the chosen elements
        if (typeof (elements) === 'string') {
            elements = document.querySelectorAll(elements);
        }

            // if we have a single DOM element, make it an array to simplify behavior
        else if (elements.tagName) {
            elements = [elements];
        }

        // add class to all chosen elements
        for (var i = 0; i < elements.length; i++) {
            // if class is not already found
            if ((' ' + elements[i].className + ' ').indexOf(' ' + myClass + ' ') < 0) {
                // add class
                elements[i].className += ' ' + myClass;
            }
        }
    },

    removeClass: function (elements, myClass) {
        // if there are no elements, we're done
        if (!elements) {
            return;
        }

        // if we have a selector, get the chosen elements
        if (typeof (elements) === 'string') {
            elements = document.querySelectorAll(elements);
        }

            // if we have a single DOM element, make it an array to simplify behavior
        else if (elements.tagName) {
            elements = [elements];
        }

        // create pattern to find class name
        var reg = new RegExp('(^| )' + myClass + '($| )', 'g');

        // remove class from all chosen elements
        for (var i = 0; i < elements.length; i++) {
            elements[i].className = elements[i].className.replace(reg, '');
        }
    },

    duplicateAsset: function (index, value, holder) {
        if (holder[index] === value) {
            return false;
        } else {
            return holder.indexOf(value) !== -1;
        }
    },

    removePersistantClassData: function (className, actualProp, prop) {
        let index = actualProp.indexOf(className);
        if (index > -1) {
            actualProp.splice(index, 1);
        }
        this.fl_changeDataProperty(prop.toLowerCase() + 'Classes',
            'coherent_attributes', this.safeArrayToString(actualProp));
    },

    dispatcheDelayedWindowEvent: function (eventType, delay) {
        setTimeout(function () {
            window.dispatchEvent(new Event(eventType));
        }, delay)
    },

    safeArrayToString: function (array) {
        if (array.length > 0) {
            return array.join(ARRAY_SPLITTER);
        }
        return "";
    },

    disableInput: function (idOrDom, shouldDisable, value) {
        const element = typeof idOrDom === 'string' ? document.getElementById(idOrDom) : idOrDom;
        const isCustomControl = element != null && typeof element.__CLCustomControl__ !== 'undefined';

        if (element && !isCustomControl) {
            element.disabled = shouldDisable;

            if (element.type === 'checkbox') {
                if (shouldDisable) {
                    element.checked = false;
                } else {
                    element.checked = value ? value : false;
                }
            } else {
                element.value = value ? value : "";
            }
        } else if (isCustomControl) {
            const customControl = element.__CLCustomControl__;

            customControl.setValue(value, true);
            customControl.setDisabled(shouldDisable);
        }
    },

    disableButton: function (idOrDom, shouldDisable) {
        var element = typeof idOrDom === 'string' ? document.getElementById(idOrDom) : idOrDom;
        element.disabled = shouldDisable;
        element.style.opacity = shouldDisable ? "0.5" : "1";
    },

    fl_cleanDataSet: function (bindGroup, set) {
        var self = this;
        if (ELEMENT_SELECTED) {
            var crunchData = function (data) {
                var dataObject = JSON.parse(data);
                for (var i = 0; i < set.length; i++) {
                    dataObject[bindGroup][set[i]] = '';
                }

                SymbolData.set(JSON.stringify(dataObject));
            };
            evalScript("get_DataProperties()", crunchData);
        }
    },

    flushPropertiesPanel: function () {
        document.getElementById("components-msg").style.display = "none";
    },

    hideDataBinding: function (state) {
        if (state) {
            $('.name-required-msg').addClass('hidden');
            document.getElementById('data-bindings-container').style.display = "block";
        } else {
            $('.name-required-msg').removeClass('hidden');
            document.getElementById('data-bindings-container').style.display = "none";
        }
    },

    compareObjScheme: function (obj1, obj2) {
        //Loop through properties in object 1
        for (let p in obj1) {
            //Check property exists on both objects
            if (obj1.hasOwnProperty(p) !== obj2.hasOwnProperty(p)) return false;
            switch (typeof (obj1[p])) {
                //Deep compare objects
                case 'object':
                    if (!this.compareObjScheme(obj1[p], obj2[p])) return false;
                    break;
                    //Compare function code
                case 'function':
                    if (typeof (obj2[p]) == 'undefined' ||
                        (p != 'compare' && obj1[p].toString() != obj2[p].toString())) return false;
                    break;
                    //Compare values
                default:
                    return true;
            }
        }

        //Check object 2 for any extra properties
        for (let p in obj2) {
            if (typeof (obj1[p]) == 'undefined') return false;
        }
        return true;
    },

    synchronizeElementDataModel: function (data) {
        let tempObj = this.cloneObj(COHERENT_META_MODEL);
        let syncElement = function (tempObj, dataObj) {
            for (let key in tempObj) {
                if (dataObj.hasOwnProperty(key) && (typeof tempObj[key] === 'object')) {
                    syncElement(tempObj[key], dataObj[key]);
                } else if (dataObj[key] === undefined) {
                    dataObj[key] = tempObj[key];
                }
            }
        };
        syncElement(tempObj, data);
    },

    cloneObj: function (obj) {
        let copy;
        // Handle the 3 simple types, and null or undefined
        if (null == obj || "object" != typeof obj) return obj;

        // Handle Date
        if (obj instanceof Date) {
            copy = new Date();
            copy.setTime(obj.getTime());
            return copy;
        }

        // Handle Array
        if (obj instanceof Array) {
            copy = [];
            for (let i = 0, len = obj.length; i < len; i++) {
                copy[i] = this.cloneObj(obj[i]);
            }
            return copy;
        }

        // Handle Object
        if (obj instanceof Object) {
            copy = {};
            for (let attr in obj) {
                if (obj.hasOwnProperty(attr)) copy[attr] = this.cloneObj(obj[attr]);
            }
            return copy;
        }

        throw new Error("Unable to copy obj! Its type isn't supported.");
    },

    applyElementData: function (dataObject, groupName) {
        let group = dataObject[groupName];

        if (!group || !this.compareObjScheme(COHERENT_META_MODEL[groupName], group)) {
            this.synchronizeElementDataModel(dataObject);

            SymbolData.set(JSON.stringify(dataObject));
            group = dataObject[groupName];
        }

        if (groupName === 'coherent_attributes') {
            const attributes = Object.keys(group);

            for (let i = 0; i < attributes.length; i++) {
                let key = attributes[i];
                if (key === 'localization_id') {
                    let el = document.getElementById('localization_id');
                    this.disableInput(el, false, dataObject[key]);
                    el.value = group[key];

                } else if (key === 'clip_aa') {
                    const el = document.getElementById('clip_aa');
                    el.removeAttribute('disabled');
                    el.checked = !!group[key];

                } else if (key.indexOf('Classes') != -1) {
                    const prop = key.replace('Classes', '').toUpperCase();

                    if (group[key].trim() !== '') {
                        window['CLASSES_' + prop] = group[key].split(ARRAY_SPLITTER);

                        const arraySize = window['CLASSES_' + prop].length;
                        const extPathsTemplate = document.querySelector('#class-input-template');
                        const extPathsInput = extPathsTemplate.content.querySelector('input');
                        const apendTo = document.querySelector('#classes-holder-' + prop.toLowerCase());

                        for (let m = 0; m < arraySize; m++) {
                            extPathsInput.value = window["CLASSES_" + prop][m];
                            let $content = extPathsTemplate.content;
                            let $clone = document.importNode($content, true);
                            createDynamicInput($clone, m, prop);
                            apendTo.appendChild($clone);
                        }

                        this.dispatcheDelayedWindowEvent('resize', 50);
                    }
                } else if (key === 'component') {
                    document.getElementById('set-as-component').checked = group[key];
                    ELEMENT_IS_COMPONENT = group[key];

                }
            }
        }
    },

    /**
     * Toggles the save preferences button.
     */
    toggleSavePrefsBtn(shouldShow) {
        if (shouldShow) {
            document.querySelector('#js-container .toggle-row').classList.remove('hidden');
        } else {
            document.querySelector('#js-container .toggle-row').classList.add('hidden');
        }
    },

    onSelectionCallback: function (element) {
        this.toggleSavePrefsBtn(false);
        let parsedData = null;
        window.CURRENT_SYMBOL_NAME = null;

        document.getElementById('invalid-iter-count').classList.add('hidden');
        $('.name-required-msg').removeClass('visible').addClass('hidden');
        document.getElementById('invalid-id-msg').style.display = 'none';
        this.hideDataBinding(true);
        this.showSceneConvert(false);
        this.showElementConvert(false);

        if (ONSEL_FOCUS_CLICK) {
            // Dispatches an event to notify all tabs to flush their views.
            document.dispatchEvent(new CustomEvent(CLEvents.RESET_VIEW));

            this.flushPropertiesPanel();
            UnitsTab.deserializeUnitsPrefs({ units: { width: '', height: '', top: '', left: '', fontSize: '' } }, true);
            this.applyElementData({ animations: COHERENT_META_MODEL.animations }, 'animations');

        }

        if (this.isJson(element)) {
            evalScript(`cl_isSelectionAvailable() ? cl_currentSelection[0].libraryItem.name : ''`, (symbolName) => {
                window.CURRENT_SYMBOL_NAME = symbolName;
            });

            parsedData = JSON.parse(element);
            this.toggleSavePrefsBtn(true);
            ELEMENT_SELECTED = true;

            this.disableInput(document.getElementById('select-position'), false, parsedData.position);
            if (ONSEL_FOCUS_CLICK) {
                this.disableInput(document.getElementById("select-position"), false, parsedData.position);
                this.applyElementData(parsedData, 'coherent_bindings');
                this.applyElementData(parsedData, 'coherent_attributes');
                this.applyElementData(parsedData, 'coherent_styles');
                this.applyElementData(parsedData, 'animations');

                UnitsTab.deserializeUnitsPrefs(parsedData, false);

                // Dispatches an event to notify all tabs for the newly selected symbol.
                document.dispatchEvent(new CustomEvent(CLEvents.SYMBOL_SELECTION, {
                    detail: parsedData
                }));

            }
            this.toggleNameDependentPanels('block');
            this.showSceneConvert(false);
            this.showElementConvert(false);
            Utils.adjustSizes();

        } else if (element === 'toolbarFlush') {
            this.flushPanels();

        } else if (element === 'notAnInstance') {
            ELEMENT_SELECTED = false;
            this.flushPropertiesPanel();
            this.showSceneConvert(false);
            this.showElementConvert(true);
            this.disableInput(document.getElementById('select-position'), true, "");

        } else if (element === 'wrongScene') {
            this.flushPropertiesPanel();
            this.showSceneConvert(true);
            ELEMENT_SELECTED = false;

        } else if (element === 'unnamed') {
            ELEMENT_SELECTED = false;
            this.flushPropertiesPanel();
            this.showElementConvert(false);
            $('.name-required-msg').removeClass('hidden').addClass('visible');
            this.toggleNameDependentPanels('none');
        }
    },

    flushPanels() {
        ELEMENT_SELECTED = false;
        this.flushPropertiesPanel();
        this.showSceneConvert(false);
        this.showElementConvert(false);
        this.disableInput(document.getElementById('select-position'), true, '');
        $('.name-required-msg').removeClass('hidden').addClass('visible');
        this.toggleNameDependentPanels('none');
        this.toggleSavePrefsBtn(false);
    },

    toggleNameDependentPanels: function (displayType) {
        document.getElementById('element-classes-container').style.display = displayType;
        document.getElementById('basic-properties-container').style.display = displayType;
        document.getElementById('data-bindings-container').style.display = displayType;
        document.getElementById('units-container').style.display = displayType;
        document.getElementById('animations').style.display = displayType;

        document.getElementById('components-msg').style.display = ELEMENT_IS_COMPONENT ? '' : 'none';
        document.getElementById('eventsApp').style.display = ELEMENT_IS_COMPONENT ? 'none' : displayType;
    },

    showSceneConvert: function (shouldShow) {
        let convertMessages = document.getElementsByClassName('enable-element-container');
        for (let i = 0; i < convertMessages.length; i++) {
            convertMessages[i].style.display = "none";
        }
        if (shouldShow) {
            document.getElementById("enable-scene-container").style.display = "block";
            document.getElementById("tabs-header").style.display = "none";
            document.getElementById("toolbar").style.display = "none";
            Router.closeAll();
        } else {
            document.getElementById("enable-scene-container").style.display = "none";
            document.getElementById("tabs-header").style.display = "flex";
            document.getElementById("toolbar").style.display = "block";
            TabsHeaderScrollbar.scrollbarVisibilityToggle();
            Router.openTab();
        }
    },

    showElementConvert: function (shouldShow, msg) {
        msg = msg || "To add properties to the selected element it should be first converted to a movie clip or a graphic. Would you like to convert it now?";
        let convertMessages = document.getElementsByClassName("enable-element-container");
        if (shouldShow) {
            Router.close([
                "eventsApp",
                "units-container",
                "data-bindings-container",
                "basic-properties-container",
                "element-classes-container",
                "animations"]);
            for (let i = 0; i < convertMessages.length; i++) {
                convertMessages[i].style.display = "block";
            }
        } else {
            document.getElementById("tabs-header").style.display = "flex";
            Router.openTab();
            for (let i = 0; i < convertMessages.length; i++) {
                convertMessages[i].style.display = "none";
            }
        }
    },

    showConvertDialog: function (shouldShow) {
        let convertMessages = document.getElementsByClassName("enable-element-container");
        if (shouldShow) {
            document.getElementById("properties-container").style.display = "none";
            document.getElementById("data-binding-container").style.display = "none";
            for (let i = 0; i < convertMessages.length; i++) {
                convertMessages[i].style.display = "block";
            }
        } else {
            document.getElementById("properties-container").style.display = "block";
            document.getElementById("data-binding-container").style.display = "block";
            for (let i = 0; i < convertMessages.length; i++) {
                convertMessages[i].style.display = "none";
            }
        }
    },

    enablePanels: function () {
        var dataAtributes = document.querySelectorAll('input[data-property-key]');
        Array.prototype.forEach.call(dataAtributes, (domEl) => {
            this.disableInput(domEl, false);
        });
    },

    fl_changeDataProperty: function (dataType, dataGroup, value, callback) {
        if (!ELEMENT_SELECTED) {
            return;
        }

        let crunchData;

        if (Array.isArray(dataType) && Array.isArray(value)) {
            crunchData = function (data) {
                if (data === 'null' || data === '0') {
                    return;
                }

                let dataObject = JSON.parse(data);

                for (let i = 0; i < dataType.length; i++) {
                    dataObject[dataGroup][dataType[i]] = value[i];
                }

                dataObject = JSON.stringify(dataObject);

                SymbolData.set(dataObject).then(callback);
            };

        } else {
            crunchData = function (data) {
                if (data === 'null' || data === '0') {
                    return;
                }

                let dataObject = JSON.parse(data);

                dataObject[dataGroup][dataType] = value;

                if (!dataObject[dataGroup]) {
                    dataObject[dataGroup] = {};
                }

                dataObject = JSON.stringify(dataObject);
                SymbolData.set(dataObject).then(callback);
            };
        }

        evalScript('get_DataProperties()', crunchData);
    },

    fl_removeProperty: function (bindProp, dataGroup) {
        let self = this;
        if (ELEMENT_SELECTED) {
            let crunchData = function (data) {
                let dataObject = JSON.parse(data);
                delete dataObject[dataGroup][bindProp];
                SymbolData.set(JSON.stringify(dataObject));
            };
            evalScript("get_DataProperties()", crunchData);
        }
    },

    isJson: function (str) {
        let object;
        try {
            object = JSON.parse(str);
        } catch (e) {
            return false;
        }
        return object;
    },

    onWindowResize: function () {
        window.addEventListener('resize', () => {
            document.dispatchEvent(new CustomEvent(CLEvents.RESIZE_INPUTS));
        });
    },

    setStyleToElements: function (elementsList, styleType, styleValue) {
        Array.prototype.forEach.call(elementsList, element => {
            element.style[styleType] = styleValue;
        });
    },

    hideHBUndupported: function () {
        const hbUnsupported = document.getElementsByClassName('hb-unsupported');
        const gtUnsupported = document.getElementsByClassName('gt-unsupported');
        const hbSuported = document.getElementsByClassName('hb-supported');
        const gtSuported = document.getElementsByClassName('gt-supported');

        csInterface.evalScript('if(fl.getDocumentDOM()){fl.getDocumentDOM().type}', type => {
            CURRENT_DOCUMENT_TYPE = type;
            if (type === 'Coherent Exporter HB') {
                this.setStyleToElements(hbSuported, 'display', '');
                this.setStyleToElements(hbUnsupported, 'display', 'none');
                this.setStyleToElements(gtUnsupported, 'display', '');
                this.setStyleToElements(gtSuported, 'display', 'none');
            } else if (type === 'Coherent Exporter GT') {
                this.setStyleToElements(hbUnsupported, 'display', '');
                this.setStyleToElements(hbSuported, 'display', 'none');
            }
            this.togglePlayButton();
        });
    },

    /**
     * Generates the outputPath for files:
     * 1. which have been saved, but don't have an output path specified
     * 2. files, which haven't been saved yet
     *
     * @param {string} documentDOMPath - the path to the document, undefined if it hasn't been saved
     * @param {function} resolve - ref to a Promise's resolve function
     * @returns {string} - resolves the promise with the value of the new folderURI
    */
    generateOutputPath(documentDOMPath, resolve) {
        if (documentDOMPath !== 'undefined') {
            documentDOMPath = documentDOMPath.replace(/\\/g, '/').split('/');
            documentDOMPath = documentDOMPath.slice(0, documentDOMPath.length - 1);
            documentDOMPath = documentDOMPath.join('/');
            resolve(documentDOMPath);

        } else {
            if (!cep_node) {
                resolve('');
                return false;
            }

            const userTempFolder = cep_node.process.env[OS.isWindows() ? 'TEMP' : 'TMPDIR'];

            const folderPath = path.join(userTempFolder, 'CLAnimateTMP');

            if (!fs.existsSync(folderPath)) {
                fs.mkdirSync(folderPath);
            }

            resolve(folderPath.replace(/\\/g, '/'));
        }
    },

    /**
     * Returns the output path specified from the customer
     * if there isn't path, it calls the generateOutputPath
     * @returns {Promise} - a promise, which will resolve to
     * the new output path
     */
    getOutputPath() {
        return new Promise((resolve, reject) => {
            evalScript('get_DocumentData()', (data) => {
                let dataObj = JSON.parse(data);
                if (!dataObj) {
                    reject('No current document!');
                    return false;
                }

                const outputPath = dataObj.outputFile;
                const hasValue = /\S/.test(outputPath);

                if (hasValue) {
                    if (typeof cep_node !== 'undefined' && fs.existsSync(outputPath) || typeof cep_node === 'undefined') {
                        resolve(outputPath);
                    } else {
                        csInterface.evalScript('fl.getDocumentDOM().path;', (documentDOMPath) => {
                            this.generateOutputPath(documentDOMPath, resolve);
                        });
                    }

                } else {
                    csInterface.evalScript('fl.getDocumentDOM().path;', (documentDOMPath) => {
                        this.generateOutputPath(documentDOMPath, resolve);
                    });
                }
            });
        });
    }
};

export default CoherentUtilities;
