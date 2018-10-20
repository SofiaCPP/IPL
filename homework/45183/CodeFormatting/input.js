import CoherentUtilities from "../CoherentUtilities";
import Select from "../components/Select/Select";
import { SUPPORTED_UNIT_TYPES } from './UnitTypes';

const DEFAULT_OPTION = Object.freeze({
    value: 'default',
    displayed: 'Use default units'
});

const SUPPORTED_UNIT_TYPES_POSITIONS = Object.assign([], SUPPORTED_UNIT_TYPES);
SUPPORTED_UNIT_TYPES_POSITIONS.push(DEFAULT_OPTION);
Object.freeze(SUPPORTED_UNIT_TYPES_POSITIONS);

const SUPPORTED_UNITS_TYPE_FONT_SIZE = Object.assign([], SUPPORTED_UNIT_TYPES_POSITIONS);
for (let i = 0; i < SUPPORTED_UNITS_TYPE_FONT_SIZE.length; ++i) {
    if (SUPPORTED_UNITS_TYPE_FONT_SIZE[i].value === 'percents') {
        SUPPORTED_UNITS_TYPE_FONT_SIZE.splice(i, 1);
        break;
    }
}
Object.freeze(SUPPORTED_UNITS_TYPE_FONT_SIZE);

const AUTO_OPTION = Object.freeze({
    value: 'auto',
    displayed: 'auto'
});

const SUPPORTED_UNITS_TYPE_DIMENSIONS = Object.assign([], SUPPORTED_UNIT_TYPES);
SUPPORTED_UNITS_TYPE_DIMENSIONS.push(AUTO_OPTION);
SUPPORTED_UNITS_TYPE_DIMENSIONS.push(DEFAULT_OPTION);
Object.freeze(SUPPORTED_UNITS_TYPE_DIMENSIONS);

const DEFAULT_UNIT_SELECTION_INDEX = SUPPORTED_UNIT_TYPES_POSITIONS.length - 1;
const DEFAULT_UNITS_SELECTION_VALUE = SUPPORTED_UNIT_TYPES_POSITIONS[DEFAULT_UNIT_SELECTION_INDEX].value;

const UNITS_SELECT_CONFIGURATIONS = Object.freeze({
    WIDTH: Object.freeze({
        sSelector: '#widthUnits',
        aOptionObj: SUPPORTED_UNITS_TYPE_DIMENSIONS,
        nSelected: DEFAULT_UNIT_SELECTION_INDEX
    }),
    HEIGHT: Object.freeze({
        sSelector: '#heightUnits',
        aOptionObj: SUPPORTED_UNITS_TYPE_DIMENSIONS,
        nSelected: DEFAULT_UNIT_SELECTION_INDEX
    }),
    TOP: Object.freeze({
        sSelector: '#topUnits',
        aOptionObj: SUPPORTED_UNIT_TYPES_POSITIONS,
        nSelected: DEFAULT_UNIT_SELECTION_INDEX
    }),
    LEFT: Object.freeze({
        sSelector: '#leftUnits',
        aOptionObj: SUPPORTED_UNIT_TYPES_POSITIONS,
        nSelected: DEFAULT_UNIT_SELECTION_INDEX
    }),
    FONTSIZE: Object.freeze({
        sSelector: '#fontSizeUnits',
        aOptionObj: SUPPORTED_UNITS_TYPE_FONT_SIZE,
        nSelected: 0
    })
});

export default class UnitsTab {
    constructor() {
        /**
         * @type {Object.<string, Select>}
         */
        UnitsTab.units = {
            width: new Select(UNITS_SELECT_CONFIGURATIONS.WIDTH),
            height: new Select(UNITS_SELECT_CONFIGURATIONS.HEIGHT),
            top: new Select(UNITS_SELECT_CONFIGURATIONS.TOP),
            left: new Select(UNITS_SELECT_CONFIGURATIONS.LEFT),
            fontSize: new Select(UNITS_SELECT_CONFIGURATIONS.FONTSIZE)
        }
        UnitsTab.addEventHandlers();
    }

    /**
     * Sets all values of the Unit Selects and writes the new values to the current selection.
     *
     * @param {Object} metadata - UnitsTab types for some item.
     * @param {boolean} disable - Whether the units Select should be disabled.
     */
    static deserializeUnitsPrefs(metadata, disable) {
        const toWriteValues = [];
        const toWriteTypes = [];

        for (let unitsType in UnitsTab.units) {
            let metaValue = metadata.units[unitsType];

            // Backwards compatability.
            if (metaValue === '' || metaValue === undefined) {
                metaValue = DEFAULT_UNITS_SELECTION_VALUE;
            }
            if (metaValue === 'px') {
                metaValue = 'pixels';
            }

            UnitsTab.units[unitsType].setValue(metaValue, true);
            UnitsTab.units[unitsType].setDisabled(disable);

            toWriteValues.push(metaValue);
            toWriteTypes.push(unitsType);
        }
    }

    /**
     * Attaches a change event listener to each UnitsTab Select that writes
     * the new units type to the current selection.
     */
    static addEventHandlers() {
        for (let unitsType in UnitsTab.units) {
            UnitsTab.units[unitsType].addEventListener('change', event => {
                CoherentUtilities.fl_changeDataProperty(
                    unitsType,
                    "units",
                    event.detail.value);
            });
        }
    }
}
