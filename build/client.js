/**
 * @overview
 * @version 0.2-beta
 * @author graham
 * @copyright 2020
 * @license Hexagonal Awareness License (HAL)
 */

  /**
  * Filters are functions that take in a state value, plus optionally a {@link Cell} instance, and return a potentially
  * modified form of that value.
  *
  * Filters can be added and removed via {@link Model#addFilter} and {@link Model#removeFilter}.
  *
  * @namespace {object} Hexular.filters
  */

  /**
  * Rules are functions that take in a {@link Cell} instance, and return a state value, typically a natural number.
  *
  * The rules provided here are for example purposes only. A somewhat more robust set of examples can be found in the
  * project's `/demo/rules.js` file.
  *
  * @namespace {object} Hexular.rules
  */

  /**
   * A selection of sundry functions with anticipated general utility.
   *
   * @namespace {object} Hexular.util
   */

var Hexular = (function () {
  const DEFAULTS = {
    // Default size for cubic (hexagonal) topology
    radius: 30,
    // Default size for offset (rectangular) topology
    rows: 60,
    cols: 60,
    // Default rule is used whenever a cell state does not have an entry in model.rules
    defaultRule: identityRule,
    // Array type to use for import/export
    arrayType: Int8Array,
    // This is only needed if one is using modFilter or certain cell/neighborhood helper functions
    numStates: 2,
    // Some functions depend on the ground state evaluating to false so changing this may be weird
    groundState: 0,
    // Used by CanvasAdapter
    cellRadius: 10,
    cellGap: 1,
    cellBorderWidth: 0,
    highlightLineWidth: 2,
    colors: [
      'transparent',
      '#ccccbb',
      '#99998f',
      '#666655',
      '#33332f',
      '#cc4444',
      '#ee7722',
      '#eebb33',
      '#66bb33',
      '#66aaaa',
      '#4455bb',
      '#aa55bb',
    ],
    backgroundColor: '#ffffff',
    defaultColor: '#ccccff',
  };

  /**
   * A collection of elements representing common hexagonal concepts for general semantic interoperability.
   *
   * @namespace {object} Hexular.enums
   */

    /**
     * Enumerator representing flat-topped, the greatest of all hexagons.
     *
     * @name TYPE_FLAT
     * @memberof Hexular.enums
     */
  const TYPE_FLAT = 0;

    /**
     * Enumerator representing pointy-topped hexagons.
     *
     * @name TYPE_POINTY
     * @memberof Hexular.enums
     */
  const TYPE_POINTY = 1;

  /**
   * A collection of mathematical properties and functions used internally, which may be of interest when extending
   * core functionality.
   *
   * @namespace {object} Hexular.math
   */

  const APOTHEM = Math.sqrt(3) / 2;

  let math = {
    apothem: APOTHEM,
    hextant: Math.PI * 2 / 6,
    inverseApothem: 1 / APOTHEM,
    vertices: [
      [-1, 0],
      [-0.5, -APOTHEM],
      [0.5, -APOTHEM],
      [1, 0],
      [0.5, APOTHEM],
      [-0.5, APOTHEM],
    ],
    /**
     * 2*2 basis matrix for converting unit cubic [u, v] coordinates to cartesian [x, y].
     *
     * @name basis
     * @type number[][]
     * @memberof Hexular.math
     */
    basis: [
      [2 * APOTHEM, APOTHEM],
      [0,           1.5]
    ],
    /**
     * 2*2 inverse basis matrix for converting unit cartesian [x, y] coordinates to cubic [u, v].
     *
     * @name invBasis
     * @type number[][]
     * @memberof Hexular.math
     */
    invBasis: [
      [1 / (2 * APOTHEM), -1 / 3],
      [0,                 2 / 3]
    ]
  };

  /**
   * Class representing a Hexular error.
   *
   * @augments Error
   */
  class HexError extends Error {}

  /**
   * @function methodNotImplemented
   *
   * @param {string} [methodName='method'] String description of method &mdash; for informational purposes only
   * @throws {HexError}
   * @memberof HexError
   */
  HexError.methodNotImplemented = (methodName = 'method') => {
    throw new HexError(`Method not implemented: "${methodName}"`);
  }

  /**
  * @function validateKeys
  *
  * @param {object} object     Object
  * @param {...string} ...args One or more string or string-coercible keys to check
  * @throws {HexError}
  * @memberof HexError
   */
  HexError.validateKeys = (obj, ...args) => {
    for (let key of args)
      if (!obj[key])
         throw new HexError(`${obj.constructor.name} requires "${key}" to be defined`);
  }

  /**
   * Abstract class representing a grid of cells connected according to some topology.
   */
  class Model {
    /**
    * Abstract constructor for creating a `Model` instance.
    *
    * @param {...object} ...args One or more settings objects to apply to model
    */
    constructor(...args) {
      let defaults = {
        /**
         * Default rule function to use for states not defined by {@link Model#rules}.
         *
         * For non-numeric cell states, arbitrary state keys can be added to this array.
         *
         * @name Model#defaultRule
         * @type function
         * @default {@link Hexular.rules.identityRule}
         */
        defaultRule: DEFAULTS.defaultRule,
        /**
         * Default numeric type for binary import and export.
         *
         * @name Model#arrayType
         * @default Int8Array
         */
        arrayType: DEFAULTS.arrayType,
        /**
         * Total number of states.
         *
         * Convenience attribute used by cell neighborhood and filter functions.
         *
         * @name Model#numStates
         * @type number
         * @default 2
         */
        numStates: DEFAULTS.numStates,
        /**
         * Default ground or "off" state for cells.
         *
         * Used by cell initialization, {@link Model#import}, and {@link Model#clear}, and
         * potentially by {@link Hexular.filters|filters}, {@link Adapter|adapters}, and other extensions.
         *
         * @name Model#groundState
         * @type number
         * @default 0
         */
        groundState: DEFAULTS.groundState,
        /**
         * Non-negative numberic value defining cell radius for spatial rendering.
         *
         * Used for determining x, y position of a cell in a given topology for e.g. rendering on a canvas. Not used
         * internally by model.
         *
         * @name Model#cellRadius
         * @type number
         * @default: 10
         * @see {@link Model#basis}
         * @see {@link Model#getCoord}
         */
        cellRadius: DEFAULTS.cellRadius,
        /**
         * Array of rule functions.
         *
         * Cells are matched with rules based on their states, with e.g. `rules[1]` being caled when
         * {@link Cell#state|cell.state} == `1`.
         *
         * @name Model#rules
         * @type function[]
         * @see {@link Hexular.rules}
         */
        rules: [],
        /**
         * List of filter functions to call on every new cell state.
         *
         * @name Model#filters
         * @type HookList
         * @default {@link Hexular.filters.modFilter|[Hexular.filters.modFilter]}
         */
        filters: new HookList(this),
        /**
         * Canonical, publicly-exposed one-dimensional array of cells in an order defined by a given subclass.
         *
         * @name Model#cells
         * @type Cell[]
         */
        cells: [],
        /**
         * Mapping of cells to [x, y] coordinates computed using {@link Model#cellRadius} and (implementation
         *-dependent) {@link Model#getCoord}.
         *
         * Like {@link Model#cellRadius} and {@link Model#basis}, this is only necessary when rendering cells in a
         * spatial context.
         *
         * @name Model#cellMap
         * @type Map
         */
        cellMap: new Map(),
        /**
         * Boolean flag that is set to true during {@link Model#step} when any {@link Cell#state} is changed, and false
         * otherwise.
         *
         * Can be used to e.g. automatically stop an auto-incrementing model when it goes "dead."
         *
         * @name Model#changed
         * @type boolean
         */
        changed: null,
      };
      Object.assign(this, defaults, ...args);
      /**
       * A 2*2 row-major transformation matrix for converting arbitrary adapter coordinates to cartesian [x, y] values.
       *
       * Derived from {@link Hexular.math.basis} scaled by {@link Model#cellRadius}.
       *
       * @name Model#basis
       * @type number[][]
       * @see {@link Model#cellRadius}
       * @see {@link Model#getCoord}
       */
      this.basis = scalarOp(math.basis, this.cellRadius);
      /**
       * Apothem computed from {@link Model#cellRadius}.
       *
       * @name Model#cellApothem
       * @type number
       *
       */
      this.cellApothem = this.cellRadius * math.apothem;
      // Add available adapter constructors as direct attributes of this instance
      Object.entries(attributes.classes.adapters).forEach(([className, Class]) => {
        this[className] = (...args) => new Class(this, ...args);
      });
    }

    /**
     * Add filter function to model.
     *
     * @param {function} filter                  Filter to add
     * @param {number} [idx=this.filters.length] Optional insertion index (defaults to end of array)
     */
    addFilter(filter, idx=this.filters.length) {
      let boundFilter = filter.bind(this);
      boundFilter.hash = this._hash(filter.toString());
      this.filters.splice(idx, 0, boundFilter);
    }

    /**
     * Remove filter function from model.
     *
     * Since filters are bound to the model, and anonymous functions lack a name, they can't be directly compared to
     * those in `this.filters`, . Thus we identify and compare functions based on a hash value derived from the string
     * version of the function. The upshot being any identically-coded functions will be equivalent.
     *
     * @param {function} filter Filter to remove
     */
    removeFilter(filter) {
      let hash = this._hash(filter.toString());
      let idx = this.filters.findIndex(((e) => e.hash == hash));
      if (idx < 0) return;
      this.filters.splice(idx, 1);
      return idx;
    }

    /**
     * Clear all filters.
     *
     */
    clearFilters() {
      while (this.filters.length)
        this.filters.pop();
    }

    /**
     * Advance state of each cell according to rule defined in {@link Model.rules|this.rules} for current state key.
     */
    step() {
      this.changed = false;
      this.eachCell((cell) => {
        let nextState
        try {
          nextState = (this.rules[cell.state] || this.defaultRule)(cell);
        }
        catch (e) {
          if (e instanceof TypeError)
            throw new HexError(`Invalid rule function for state "${cell.state}"`);
          else
            throw e;
        }
        cell.nextState = this.filters.call(nextState, cell);
        if (!this.changed && cell.nextState != cell.state)
          this.changed = true;
      });
      this.eachCell((cell) => {
        cell.state = cell.nextState;
      });
    }

    /**
     * Reset each cell state to {@link Model.groundState|this.groundState}.
     */
    clear() {
      this.eachCell((cell) => {
        cell.state = this.groundState;
      });
    }

    /**
     * Set {@link Cell#neighborhood} for each cell to given value.
     *
     * @param {number} neighborhood One of the natural numbers [6, 12, 18, 7, 13, 19].
     */
    setNeighborhood(neighborhood) {
      this.eachCell((cell) => {
        cell.neighborhood = neighborhood;
      })
    }

    /**
     * Call a given function for each cell in a model, and return an array of that function's return values.
     *
     * This is essentially `forEach` on {@link Model#cells} but with array comprehension behavior.
     *
     * @param {function} fn Function to call for each {@link Cell|cell}, taking the cell as an argument
     * @return {number[]}    Array of return values with same size as {@link Model#cells|this.cells}
     */
    eachCell(fn) {
     let a = new Array(this.cells.length);
     for (let i = 0; i < this.cells.length; i++) {
       a[0] = fn(this.cells[i]);
     }
     return a;
    }

    /**
     * Build cell map using {@link Model#cellRadius} and `Model` subclass implementation of {@link Model#eachCoord}.
     *
     * This should optionally be called by an adapter, &c., that wishes to use canonical cartesian coordinates for
     * cells. This method should be idempotent.
     */
    buildCellMap() {
      this.cellMap.clear();
      this.eachCell((cell) => {
        this.cellMap.set(cell, this.getCoord(cell));
      });
    }

    /**
     * Call a given function for each coordinate defined by a model's topology.
     *
     * This is typically used by a model's constructor to instantiate cells, but should be exposed externally as well.
     *
     * @param {function} fn Function to call for each coordinate, taking a coordinate argument that e.g. is used to
     *                       construct {@link Cell#coord}
     */
    eachCoord(fn) {
      HexError.methodNotImplemented('eachCoord');
    }

    /**
     * Get coordinates of cell according to {@link Model#cellRadius}, relative to an origin defined by a subclass.
     *
     * @param {Adapter} adapter An adapter instance with {@link Model#cellRadius} and {@link Model#basis} defined
     * @param {Cell} cell       The cell to position
     * @return {number[]}       The cell's [x, y] position in the adapter's frame of reference
     */
    getCoord(adapter, cell) {
      HexError.methodNotImplemented('getCoord');
    }

    /**
     * Find cell at given cubic coordinates in model.
     *
     * There is no "topologically agnostic" way to spatially locate a cell in any given model. Thus, we leave the
     * onus on specific `Model` subclasses to convert cubic coordinates to their internal coordinate system, and allow
     * e.g. {@link Adapter} subclass instances to look up cells spatially using this convention.
     *
     * @param {number[]} coord Array of [u, v, w] coordinates
     * @return {Cell}           Cell at coordinates, or null
     */
    cellAtCubic([u, v, w]) {
      HexError.methodNotImplemented('cellAtCubic');
    }

    /**
     * Get cell at specific [x, y] coordinates.
     *
     * This is used by Hexular Studio and potentially other display-facing applications for locating a cell from e.g.
     * a user's cursor position using {@link Model#cellRadius}.
     *
     * @param {number[]} coord An [x, y] coordinate tuple
     * @return {Cell}           The cell at this location, or null
     * @see {@link Hexular.math.cartesianToCubic}
     * @see {@link Hexular.math.roundCubic}
     * @see {@link Model#cellAtCubic}
     */
    cellAt([x, y]) {
      // First convert to cubic coords
      let rawCubic = cartesianToCubic([x, y]);
      let cubic = roundCubic(rawCubic, this.cellRadius);
      let cell = this.cellAtCubic(cubic);
      return cell;
    }

    /**
     * Export cell states to a typed byte array.
     *
     * Neither this method nor its counterpart {@link Model#import} deals with other aspects of models or cells,
     * such as {@link Model#rules|rules} or {@link Cell#neighborhood|neighborhoods}, and will not prove effective,
     * under the default settings, for non-numeric states or states outside the range -128...128.
     *
     * @return {TypedArray} Typed array of cell states
     * @see {@link Model#arrayType})
     * @see {@link Model#import}
     */
    export() {
      let array = this.arrayType.from(this.cells.map((e) => e.state));
      return array;
    }

    /**
     * Import cell states from typed or untyped array.
     *
     * @param {TypedArray|Array} array Any array of cell states
     * @see {@link Model#arrayType})
     * @see {@link Model#export}
     */
    import(array) {
      this.cells.forEach((cell, idx) => {
        cell.state = array[idx] || this.groundState;
      });
    }

    /**
     * Internal hashing function to track bound functions. Not actually important.
     *
     * @param {string} str Some string
     * @return {string}     Chunked, summed mod 256 hexadecimal string
     */
    _hash(str) {
      let bytes = new Uint8Array(str.split('').map((e) => e.charCodeAt(0)));
      let chunkSize = Math.max(2, Math.ceil(bytes.length / 16));
      let chunked = bytes.reduce((a, e, i) => {
        a[Math.floor(i / chunkSize)] += e;
        return a;
      }, Array(Math.ceil(bytes.length / chunkSize)).fill(0));
      return chunked.map((e) => ('0' + (e % 256).toString(16)).slice(-2)).join('');
    }
  }

/**
 * Class representing an offset, i.e. rectangular, topology.
 *
 * In an offset topology, cells describe a `cols * rows` grid where every other row is staggered one apothem-length
 * along the x axis. This is useful when trying to fit a grid into a rectangular box, but may cause undesirable
 * wraparound effects. (These effects may be mitigated by using {@link Hexular.filters.edgeFilter}.)
 *
 * @augments Model
 */
  class OffsetModel extends Model {
    /**
    * Creates `OffsetModel` instance
    *
    * @param {...object} ...args One or more settings objects to apply to model
    */
    constructor(...args) {
      super(...args);
      let defaults = {
        /**
         * @name OffsetModel#cols
         * @type number
         * @default 60
         */
        cols: DEFAULTS.cols,
        /**
         * @name OffsetModel#rows
         * @type number
         * @default 60
         */
        rows: DEFAULTS.rows,
        cells: [],
      };
      Object.assign(this, defaults, args);
      HexError.validateKeys(this, 'rows', 'cols');
      let rows = this.rows, cols = this.cols;
      this.eachCoord(([i, j]) => {
        // Being on an edge affects draw actions involving neighbors
        let edge = (i == 0 || i == this.cols - 1 || j == 0 || j == rows - 1);
        this.cells.push(new Cell(this, [i, j], {edge}));
      });

      // Connect simple neighbors
      this.eachCell((cell) => {
        let [i, j] = cell.coord;
        let upRow = mod(j - 1, rows);
        let downRow = mod(j + 1, rows);
        let offset = downRow % 2;

        cell.nbrs[1] = this.cells[downRow * cols + mod(i - offset + 1, cols)];
        cell.nbrs[2] = this.cells[j * cols + mod(i + 1, cols)];
        cell.nbrs[3] = this.cells[upRow * cols + mod(i - offset + 1, cols)];
        cell.nbrs[4] = this.cells[upRow * cols + mod(i - offset, cols)];
        cell.nbrs[5] = this.cells[j * cols + mod(i - 1, cols)];
        cell.nbrs[6] = this.cells[downRow * cols + mod(i - offset, cols)];
      });

      // Connect extended neighbors
      this.eachCell((cell) => {
        cell.extendNeighborhood();
      });
    }

    eachCoord(fn) {
      for (let j = 0; j < this.rows; j++) {
        for (let i = 0; i < this.cols; i++) {
          if (fn([i, j]) === false) return false;
        }
      }
      return true;
    }

    getCoord(cell) {
      let r = this.cellRadius;
      let [i, j] = cell.coord;

      // Like converting to cubic coords but mod 2 wrt x offset
      let x = this.basis[0][0] * i + this.basis[0][1] * (j % 2);
      let y = this.basis[1][0] * i + this.basis[1][1] * j;
      return [x, y];
    }

    cellAtCubic([u, v, w]) {
      // For offset, we shift every two rows to the left
      v += u >> 1;
      let cell = this.cells[u * this.cols + v];
      return cell;
    }
  }

  /**
   * Class representing a hexagonal model with cells addressed using cubic coordinates.
   *
   * Implements a regularly-hexagonal grid of cells addressed by coordinates `[u, v, w]`. The cell at the origin is
   * designated `[0, 0, 0]`, with all cell coordinate tuples summing to zero. In the default display provided by
   * {@link CanvasAdapter}, `u` points up, `v` points to the right, and `w` points to the left.
   *
   * For more information on this system, and how it translates to other coordinate systems, please see the excellent
   * article [Hexagonal Grids]{@link https://www.redblobgames.com/grids/hexagons/} from Red Blob Games.
   *
   * @augments Model
   */
  class CubicModel extends Model {
    /**
    * Creates `CubicModel` instance.
    *
    * @param {...object} ...args One or more settings objects to apply to model
    */
    constructor(...args) {
      super(...args);
      let defaults = {
        /**
         * @name CubicModel#radius
         * @type number
         * @default 30
         */
        radius: DEFAULTS.radius,
      };
      Object.assign(this, defaults, ...args);
      HexError.validateKeys(this, 'radius');
      this.size = this.radius * (this.radius - 1) * 3 + 1;
      let max = this.max = this.radius - 1;
      let cols = this.cols = this.radius * 2 - 1;
      this.rhombus = Array(cols * 2).fill(null);

      this.eachCoord(([u, v, w]) => {
          // Being on an edge affects draw actions involving neighbors
          let edge = absMax(u, v, w) == max;
          this.rhombus[u * cols + v] = new Cell(this, [u, v, w], {edge});
      });

      // A hack for the trivial case
      if (this.radius == 1) {
        this.rhombus[0].nbrs.fill(this.rhombus[0]);
      }
      // Otherwise connect simple neighbors
      else {
        Object.values(this.rhombus).filter((e) => e).forEach((cell) => {
          for (let i = 0; i < 6; i++) {
            let dir1 = i >> 1;
            let dir2 = (dir1 + 1 + i % 2) % 3;
            let dir3 = (dir1 + 1 + +!(i % 2)) % 3;
            let nbr = cell.coord.slice();
            nbr[dir1] += 1;
            nbr[dir2] -= 1;
            nbr[dir3] = -nbr[dir1] - nbr[dir2];
            for (let dir of [dir1, dir2, dir3]) {
              if (Math.abs(nbr[dir]) > max) {
                let sign = Math.sign(nbr[dir]);
                let dirA = (dir + 1) % 3;
                let dirB = (dir + 2) % 3;
                nbr[dir] -= sign * cols;
                nbr[dirA] += sign * max;
                nbr[dirB] = -nbr[dir] - nbr[dirA];
              }
            }
            cell.nbrs[1 + (i + 5) % 6] = this.rhombus[nbr[0] * cols + nbr[1]];
          }
        });
      }

      /**
       * `CubicModel` orders its `cells` array in rings from the center out, starting with a zero-indexed origin cell.
       * This allows cell states to be backed up and restored via {@link Model#export} and {@link Model#import} across
       * differently-sized maps. Cells always remain centered and in the correct order, though a smaller map will
       * truncate cells outside of its radius.
       *
       * @name CubicModel.cells
       * @type Cell[]
       */
      this.cells = hexWrap(this.rhombus[0], this.radius);

      // Connect extended neighbors
      this.eachCell((cell) => {
        cell.extendNeighborhood();
      });
    }

    eachCoord(fn) {
      for (let u = -this.max; u < this.radius; u++) {
        for (let v = -this.max; v < this.radius; v++) {
          let w = -u - v;
          if (Math.abs(w) > this.max) continue;
          if (fn([u, v, -u - v]) === false) return false;
        }
      }
      return true;
    }

    getCoord(cell) {
      let r = this.cellRadius;
      let [u, v, w] = cell.coord;

      let [x, y] = matrixMult(this.basis, [v, u]);
      return [x, y];
    }

    cellAtCubic([u, v, w]) {
      if (absMax(u, v, w) > this.max)
        return null;
      let cell = this.rhombus[u * this.cols + v];
      return cell;
    }
  }

  /**
   * Class representing a cell.
   */
  class Cell {
    /**
     * Creates `Cell` instance.
     *
     * @param {Model} model       Model for populating {@link Cell#model|this.model}
     * @param {number[]} coord    Coordinate array for populating {@link Cell#coord|this.coord}
     * @param {...object} ...args One or more settings objects to apply to cell
     */
    constructor(model, coord, ...args) {
      let defaults = {
        /**
         * {@link Model} instance associated with this cell (typically also its instantiator).
         *
         * @name Cell#model
         * @type Model
         */
        model,
        /**
         * A coordinate vector whose shape and range are determined by the topology implemented in {@link Cell#model}.
         *
         * @name Cell#coord
         * @type number[]
         */
        coord,
        /**
         * Cell state, used to look up rule index on each {@link Model#step}.
         *
         * @name Cell#state
         * @type number
         * @default {@link Model#groundState|this.model.groundState}
         * */
        state: model.groundState,
        /**
         * Used by {@link Model#step} when calculating new  cell states.
         *
         * @name Cell#nextState
         * @type number
         */
        nextState: 0,
        /**
         * Numeric 19-element array with entries for the cell itself and its 18 nearest neighbors.
         *
         * - Entry 0 corresponds to the cell itself
         * - Entries 1-6 correspond to the cell's 6 nearest neighbors, progressing in a continuous arc
         * - Entries 7-12 correspond to those cells one edge-length from the cell, where `nbrs[7]` corresponds to the
         *   cell touching `nbrs[1]` in the opposite direction the first ring progresses, but progressing in the same
         *   direction as the first ring
         * - Entries 13-18 correspond to cells one full cell from the cell, where `nbrs[13]` corresponds to the cell
         *   touching `nbrs[1]` opposite the home cell, also progressing in the same direction as the other two
         *
         * That is, we have three rings where the first neighbor in each ring forms a line zigzagging away from the
         * home cell. This arrangement allows successive neighborhoods to be iterated through contiguously using the
         * same array.
         *
         * @name Cell#nbrs
         * @type number
         */
        nbrs: new Array(19).fill(null),

        /**
         * Value indicating which entry in {@link Cell#with} to look to when calling cell-level helper
         * functions, e.g. {@link Cell#count}, &c.
         *
         * @name Cell#neighborhood
         * @type number
         * @default 6
         */
        neighborhood: 6,
      };
      Object.assign(this, defaults, ...args);
      this.nbrs[0] = this;
      /**
       * Array of {@link Neighborhood} instances, for efficiently calling helper methods over defined neighborhoods.
       *
       * @name Cell#with
       * @type Neighborhood[]
       * @see {@link Cell#neighborhood}
       */
      this.with = {
        6: new Neighborhood(this, 1, 7),
        12: new Neighborhood(this, 1, 13),
        18: new Neighborhood(this, 1, 19),
        7: new Neighborhood(this, 0, 7),
        13: new Neighborhood(this, 0, 13),
        19: new Neighborhood(this, 0, 19),
      };
    }

    /**
     * Builds out {@link Cell#nbrs|this.nbrs[7:19]} after [1:7} have been populated by {@link Model|this.model}.
     */
    extendNeighborhood() {
      for (let i = 1; i < 7; i++) {
        let source12 = 1 + (i + 4) % 6;
        this.nbrs[i + 6] = this.nbrs[i].nbrs[source12];
        this.nbrs[i + 12] = this.nbrs[i].nbrs[i];
      }
    }

    /**
     * Shortcut for {@link Neighborhood#nbrSlice|this.with[this.neighborhood].nbrSlice}.
     *
     * @readonly
     */
    get nbrSlice() { return this.with[this.neighborhood].nbrSlice; }

    /**
     * Shortcut for {@link Neighborhood#total|this.with[this.neighborhood].total}.
     *
     * @readonly
     */
    get total() { return this.with[this.neighborhood].total; }
    /**
     * Shortcut for {@link Neighborhood#count|this.with[this.neighborhood].count}.
     *
     * @readonly
     */
    get count() { return this.with[this.neighborhood].count; }
    /**
     * Shortcut for {@link Neighborhood#average|this.with[this.neighborhood].average}.
     *
     * @readonly
     */
    get average() { return this.with[this.neighborhood].average; }
    /**
     * Shortcut for {@link Neighborhood#min|this.with[this.neighborhood].min}.
     *
     * @readonly
     */
    get min() { return this.with[this.neighborhood].min; }
    /**
     * Shortcut for {@link Neighborhood#max|this.with[this.neighborhood].max}.
     *
     * @readonly
     */
    get max() { return this.with[this.neighborhood].max; }
    /**
     * Shortcut for {@link Neighborhood#histogram|this.with[this.neighborhood].histogram}.
     *
     * @readonly
     */
    get histogram() { return this.with[this.neighborhood].histogram; }
    /**
     * Shortcut for {@link Neighborhood#map|this.with[this.neighborhood].map}.
     *
     * @readonly
     */
    get map() { return this.with[this.neighborhood].map; }
  }

  /**
   * Class representing a neighborhood around a cell.
   *
   * The {@link Cell#nbrs} array contains 19 entries, starting with the cell itself. By selecting particular subsets of
   * this array, we can confine our iteration to the 6, 12, or 18 nearest neighbors, with or without the cell itself.
   */
  class Neighborhood {
    /**
     * Creates `Neighborhood` instance.
     *
     * @param {Cell} cell  Parent cell, usually instantiator of neighborhood
     * @param {number} min Minimum index (inclusive) of neighborhood in {@link Cell#nbrs}.
     * @param {number} max Maximum index (exclusive) of neighborhood in {@link Cell#nbrs}.
     */
    constructor(cell, min, max) {
      this.cell = cell;
      this.nbrs = cell.nbrs;
      this.minIdx = min;
      this.maxIdx = max;
      this.length = max - min;
    }

    /**
     * Convenience method for returning limited neighbor array.
     *
     * For spatial efficiency purposes we don't keep an internal slice {@link Cell#nbrs}, but this may be useful for
     * e.g. writing certain drawing extensions, etc.
     *
     * @return {Cell[]} Array of cells in this neighborhood
     * @readonly
     */
    get nbrSlice() {
      return this.nbrs.slice(this.minIdx, this.maxIdx);
    }

    /**
     * Cumulative total of all neighboring states.
     *
     * @return {number}
     * @readonly
     */
    get total() {
      let a = 0;
      for (let i = this.minIdx; i < this.maxIdx; i++)
        a += this.nbrs[i].state;
      return a;
    }

    /**
     * Count of all activated (state > 0) neighbors.
     *
     * @return {number}
     * @readonly
     */
    get count() {
      let a = 0;
      for (let i = this.minIdx; i < this.maxIdx; i++)
        a += this.nbrs[i].state ? 1 : 0;
      return a;
    }

    /**
     * Return average of neighbor states.
     *
     * @return {number}
     * @readonly
     */
    get average() {
      let a = 0;
      for (let i = this.minIdx; i < this.maxIdx; i++)
        a += this.nbrs[i].state;
      return Math.floor(a / this.length);
    }

    /**
     * Get maximum neighbor state.
     *
     * @return {number}
     * @readonly
     */
    get max() {
      let a = -Infinity;
      for (let i = this.minIdx; i < this.maxIdx; i++)
        if (this.nbrs[i].state > a)
          a = this.nbrs[i].state;
      return a;
    }

    /**
     * Get minimum neighbor state.
     *
     * @return {number}
     * @readonly
     */
    get min() {
      let a = Infinity;
      for (let i = this.minIdx; i < this.maxIdx; i++)
        if (this.nbrs[i].state < a)
          a = this.nbrs[i].state;
      return a;
    }

    /**
     * A `numStates`-sized array containing neighbor counts for that state.
     *
     * @return {number[]}
     * @readonly
     */
    get histogram() {
      let a = Array(this.cell.model.numStates).fill(0);
      for (let i = this.minIdx; i < this.maxIdx; i ++)
        if (this.nbrs[i].state < a.length)
          a[this.nbrs[i].state] += 1;
      return a;
    }

    /**
     * Array of cell states in neighborhood.
     *
     * @return {number[]}
     * @readonly
     */
    get map() {
      let a = [];
      for (let i = this.minIdx; i < this.maxIdx; i++) {
        a.push(this.nbrs[i].state);
      }
      return a;
    }
  }

  /**
   * Class representing a list of callback hooks
   *
   * This class extends `Array`, and we can use standard array methods &mdash; e.g. `push`, &c. &mdash; to populate it.
   *
   * @augments Array
   */
  class HookList extends Array {
    /**
     * Creates `HookList` instance.
     *
     * @param {*} owner              Object or value for populating {@link HookList#owner|this.owner}
     * @param {function[]} functions Optional list of functions to add
     */
    constructor(owner, functions=[]) {
      super();
      /**
       * Object or value to be bound to functions in hook list.
       *
       * Typically a class instance. Overwriting this will have no effect on functions already in the list.
       *
       * @name HookList#owner
       */
      this.owner = owner;

      this.replace(functions)
    }

    /**
     * Convenience method for removing all existing functions and optionally adding new ones.
     *
     * @param {function[]} functions List of new functions to add
     */
    replace(functions=[]) {
      this.length = 0;
      for (let fn of functions)
        this.push(fn);
    }

    /**
     * Convenience method for removing member functions with filter function.
     *
     * @param {function} fn Filter function taking a member function and returning a boolean value
     * @return {function[]} Member functions removed during filtering
     */
    remove(fn) {
      let removed = [];
      this.replace(this.filter((e) => {
        let val = fn(e);
        val || removed.push(e);
        return val;
      }));
      return removed;
    }

    /**
     * Convenience method for rotating callback order.
     *
     * Sometimes a drawing callback is in the wrong place wrt others. This is essentially a wrapper for
     * `hookList.unshift(hookList.pop())` (default behavior) and associated operations.
     *
     * @param {number} [n=1] Negative/positive offset
     */
    rotate(n=1) {
      if (n > 0) {
        let slice = this.splice(this.length -n);
        this.replace(slice.concat(this));
      }
      else if (n < 0) {
        let slice = this.splice(0, -n);
        this.replace(this.concat(slice));
      }
    }

    /**
     * Call each function entry in hook list, bound to {@link HookList#owner|this.owner}.
     *
     * The first function is called with the arguments as given to this method. When a called
     * function returns a value besides `undefined`, `val` is set to that value for the next
     * function. The final state of `val` is returned at the end of the iteration.
     *
     * Thus, if constituent functions return a value, this operation serves to "filter" a value
     * from one function to the other. Conversely, if constituent functions do not return an
     * explicit value, this method essentially iterates over these functions with identical
     * arguments.
     *
     * The former mechanism is used by {@link Model#filters}, while the latter is used by
     * {@link CanvasAdapter#onDrawSelector}, and, when drawing individual cells, by {@link CanvasAdapter#onDrawCell}.
     *
     * @param {*} val        First argument to be passed to at least initial function
     * @param {...*} ...args Additional arguments to pass to each hook function
     * @return {*}            Return value of last hook function called, or original `val`
     */
    call(val, ...args) {
      for (let i = 0; i < this.length; i++) {
        let newVal = this[i].call(this.owner, val, ...args);
        val = newVal === undefined ? val : newVal;
      }
      return val;
    }

    /**
     * Call each function entry for every value in the given array, completing each function for all elements in the
     * array before moving on to the next.
     *
     * Used by {@link CanvasAdapter#draw} to finish each successive drawing function for all cells in turn, allowing
     * more complex intercellular drawings.
     *
     * @param {array} array       Array of values to pass to hook to functions
     * @param {...object} ...args Additional arguments to pass to each hook function
     */
    callParallel(array, ...args) {
      for (let i = 0; i < this.length; i++) {
        for (let j = 0; j < array.length; j++) {
          this[i].call(this.owner, array[j], ...args);
        }
      }
    }
  }

  /**
   * Abstract class representing an adapter.
   *
   * This doesn't really do much. The minimal adapter interface may change in the future.
   */
  class Adapter {
    /**
     * Creates `Adapter` instance.
     *
     * @param {Model} model       Model to associate with this adapter
     * @param {...object} ...args One or more settings objects to apply to adapter
     */
    constructor(model, ...args) {
      /**
       * `Model` instance to associate with this adapter.
       *
       * In the present implementation, this only needs to be a one-way relationship &mdash; models have no explicit
       * knowledge of adapters accessing them, though they can be instantiated via `model.ClassName()`, omitting
       * the `model` argument that would normally be passed to the constructor.
       *
       * @name Adapter#model
       * @type Model
       */
       this.model = model;
       Object.assign(this, ...args);
       HexError.validateKeys(this, 'model');
    }
  }

  /**
   * Class connecting a user agent canvas context to a model instance.
   *
   * This class is closely tailored to the needs of the Hexular Studio client, and probably does not expose the ideal
   * generalized interface for browser-based canvas rendering.
   *
   * Its functionality is tailored for multiple roles with respect to browser canvas drawing:
   *   - Drawing all cell states, using a list of functions applied in parellel to all cells, one at a time
   *   - Drawing one or more isolated selectors on a canvas to denote selected or otherwise highlighted cells
   *   - Drawing one or more cell states given in a separate {@link CanvasAdapter#stateBuffer|stateBuffer}, which can
   *     then be retrieved and written to underlying cell states
   *
   * All these modalities are employed by Hexular Studio using two such adapters &mdash; a foreground for selection and
   * tool paint buffering, and a background for current, canonical cell state. (This is a change from the original 2017
   * version, which used just a single canvas and a somewhat awkward raster buffer for storing the unselected drawn
   * state of the cell and then redrawing it when the selection changed. This more or less worked but led to occasional
   * platform-specific artifacts. At any rate, one can easily override the default selector-drawing behavior and use a
   * single canvas if desired.
   *
   * Note that all static methods are intended to be bound to a particular instance via {@link CanvasAdapter#onDraw}
   * and {@link CanvasAdapter#onDrawCell}, and thus should be treated as instance methods wrt `this` binding, &c. See
   * {@link HookList} for more details.
   *
   * @augments Adapter
   */
  class CanvasAdapter extends Adapter {
    /**
     * Creates `CanvasAdapter` instance.
     *
     * Requires at least {@link CanvasAdapter#context} to be given in `...args` settings.
     *
     * @param {Model} model       Model to associate with this adapter
     * @param {...object} ...args One or more settings objects to apply to adapter
     */
    constructor(model, ...args) {
      super(model);
      let defaults = {
        /**
         * Array of CSS hex or RGB color codes for cell fill color.
         *
         * @name CanvasAdapter#fillColors
         * @type string[]
         * @default Hexular.DEFAULTS.colors
         */
        fillColors: DEFAULTS.colors,
        /**
         * Array of CSS hex or RGB color codes for cell line stroke color, if applicable.
         *
         * @name CanvasAdapter#strokeColors
         * @type string[]
         * @default Hexular.DEFAULTS.colors
         */
        strokeColors: DEFAULTS.colors,
        /**
         * @name CanvasAdapter#defaultColor
         * @type string
         * @default #ccccff
         */
        defaultColor: DEFAULTS.defaultColor,
        /**
         * @name CanvasAdapter#backgroundColor
         * @type string
         * @default #ffffff
         */
        backgroundColor: DEFAULTS.backgroundColor,
        /**
         * @name CanvasAdapter#cellRadius
         * @type number
         * @default 10
         */
        cellRadius: DEFAULTS.cellRadius,
        /**
         * @name CanvasAdapter#cellGap
         * @type number
         * @default 1
         */
        cellGap: DEFAULTS.cellGap,
        /**
         * @name CanvasAdapter#cellBorderWidth
         * @type number
         * @default 0
         */
        cellBorderWidth: DEFAULTS.cellBorderWidth,
        /**
        * @name CanvasAdapter#context
        * @type 2DCanvasRenderingContext2D
        */
        context: null,

        /**
         * @name CanvasAdapter#stateBuffer
         * @type Map
         */
        stateBuffer: new Map(),
      };
      Object.assign(this, defaults, ...args);
      HexError.validateKeys(this, 'context');

      // Build cell map if not already built
      this.model.buildCellMap();
      // Compute math stuff
      this.updateMathPresets();

      /**
       * @name CanvasAdapter#onDraw
       * @type HookList
       * @default []
       */
      this.onDraw = new HookList(this);
      /**
       * @name CanvasAdapter#onDrawCell
       * @type HookList
       * @default {@link CanvasAdapter#drawfilledPointyHex|[this.defaultDawCell]}
       */
      this.onDrawCell = new HookList(this);
      this.onDrawCell.push(this.drawFilledPointyHex);
    }

    /**
     * Precompute math parameters using principally {@link Model#cellRadius}.
     */
    updateMathPresets() {
      this.cellRadius = this.model.cellRadius;
      this.innerRadius = this.cellRadius - this.cellGap / (2 * math.apothem);
      this.flatVertices = scalarOp(math.vertices, this.innerRadius);
      this.pointyVertices = scalarOp(math.vertices.map(([x, y]) => [y, x]), this.innerRadius);
    }

    /**
     * Draw all cells on {@link CanvasAdapter#context} context.
     *
     * Calls all functions in {@link CanvasAdapter#onDraw|this.onDraw} after clearing canvas but before drawing cells.
     */
    draw() {
      this.cells = null;
      this.clear();
      this.onDraw.call();
      this.onDrawCell.callParallel(this.cells || this.model.cells);
    }

    /**
     * Clear canvas context
     *
     * When used with {@link CubicModel}, which is centered on the origin, we assume the context has been translated
     * to the center of its viewport. This is neither necessary nor assumed for other models though. Thus we simply
     * save the current transformation state, clear the visible viewport, and then restore the original transform.
     */
    clear() {
      this.context.save();
      this.context.setTransform(1, 0, 0, 1, 0, 0);
      this.context.clearRect(0, 0, this.context.canvas.width, this.context.canvas.height);
      this.context.restore();
    }

    /**
     * Draw individual cell.
     *
     * Calls every method of {@link CanvasAdapter#onDrawCell} with the given cell.
     *
     * This was originally called by {@link CanvasAdapter#draw}, but is now a standalone utility method.
     *
     * @param {Cell} cell The cell to draw
     */
    drawCell(cell) {
      this.onDrawCell.call(cell);
    }

  /**
   * Default method to draw cell based on state in {@link CanvasAdapter#stateBuffer|this.stateBuffer}.
   *
   * Used for drawing new cell state segments, and then applying the changes to the underlying model as an atomic,
   * batch operation. This is used by e.g. painting tools in Hexular Studio.
   *
   * @param {Cell} cell The cell being drawn
   */
    defaultDrawBuffer(cell) {
      let state = this.stateBuffer.get(cell);
      let color = this.fillColors[state];
      if (!color)
        color = this.defaultColor;
      else if (color == 'transparent'
        || color.length == '9' && color.slice(-2) == '00'
        || color.length == '5' && color.slice(-1) == '0'
        || color.length && color.slice(-2) == '0)')
        color = this.backgroundColor;
      if (color) {
        this.context.fillStyle = color;
        this.drawPath(cell);
        this.context.fill();
      }
    }

    /**
     * Internal method used to draw hexes for both selectors and cells.
     *
     * @param {Cell} cell The cell being drawn
     */
    drawPath(cell, path=this.pointyVertices) {
      const [x, y] = this.model.cellMap.get(cell);
      let ctx = this.context;
      ctx.beginPath();
      ctx.moveTo(x + path[0][0], y + path[0][1]);
      for (let i = 1; i < path.length; i++)
        ctx.lineTo(x + path[i][0], y + path[i][1]);
      ctx.closePath();
    }

    /**
     * Utility function for drawing arbitrary hexagon
     *
     * @param {Cell|number[]} locator           The cell at the position to be drawn, or an [x, y] coordinate tuple
     * @param {number} radius                   The hexagon's radius
     * @param {object} opts                     Optional arguments specifying e.g. stroke, fill, &c.
     * @param {boolean} [opts.stroke=false]     Whether to draw stroke
     * @param {boolean} [opts.fill=false]       Whether to draw fill
     * @param {boolean} [opts.strokeStyle=null] Stroke style
     * @param {boolean} [opts.fillStyle=null]   Fill style
     */
    drawHexagon(locator, radius, opts={}) {
      let defaults = {
        type: TYPE_POINTY,
        stroke: false,
        fill: false,
        strokeStyle: null,
        lineWidth: 0,
        fillStyle: null,
      };
      opts = Object.assign(defaults, opts);
      const [x, y] = locator instanceof Cell ? this.model.cellMap.get(locator) : locator;
      let ctx = this.context;
      let path = opts.type == TYPE_POINTY ? math.pointyVertices : math.flatVertices;
      path = scalarOp(path, radius);
      ctx.beginPath();
      ctx.moveTo(x + path[0][0], y + path[0][1]);
      for (let i = 1; i < path.length; i++)
        ctx.lineTo(x + path[i][0], y + path[i][1]);
      ctx.closePath();
      if (opts.fill) {
        ctx.fillStyle = opts.fillStyle;
        ctx.fill();
      }
      if (opts.stroke) {
        ctx.strokeStyle = opts.strokeStyle;
        ctx.lineWidth = opts.lineWidth;
        ctx.stroke();
      }
    }

    /**
     * Internal method used to draw circle at cell's position using {#link Model#cellRadius}.
     *
     * Convenience method for use by optional or custom drawing callbacks.
     *
     * @param {Cell} cell The cell being drawn
     */
    drawCircle(cell, radius=this.innerRadius) {
      const [x, y] = this.model.cellMap.get(cell);
      let ctx = this.context;
      ctx.beginPath();
      ctx.arc(x, y, radius, 0, Math.PI * 2);
    }


    /**
     * Default cell drawing method.
     *
     * @param {Cell} cell           The cell being drawn
     * @param {string} [style=null] Optional argument when called directly specifying fill style
     */
    drawFilledPointyHex(cell, style) {
      this.context.fillStyle = style || this.fillColors[cell.state] || this.defaultColor;
      this.drawPath(cell);
      this.context.fill();
    }

    /**
     * Draw cell outline.
     *
     * An alternative drawing method that uses {@link CanvasAdapter#cellBorderWidth|this.cellBorderWidth} to draw an
     * outline instead of a filled hex.
     *
     * @param {Cell} cell                                   The cell being drawn
     * @param {string} [style=this.strokeColors[cell.state]] Optional stroke style
     * @param {number} [lineWidth=this.cellBorderWidth]     Optional line width
     */

    drawOutlinePointyHex(cell, style, lineWidth=this.cellBorderWidth) {
      if (lineWidth == 0)
        return;
      this.context.strokeStyle = style || this.strokeColors[cell.state] || this.defaultColor;
      this.context.lineWidth = lineWidth;
      this.drawPath(cell);
      this.context.stroke();
    }

    /**
     * Draw filled, flat-top cell.
     *
     * @param {Cell} cell                                  The cell being drawn
     * @param {string} [style=this.fillColors[cell.state]] Optional stroke style
     */
    drawFilledFlatHex(cell, style) {
      this.context.fillStyle = style || this.fillColors[cell.state] || this.defaultColor;
      this.drawPath(cell, this.flatVertices);
      this.context.fill();
    }

    /**
     * Draw flat-topped cell outline.
     *
     * @param {Cell} cell                                   The cell being drawn
     * @param {string} [style=this.strokeColors[cell.state]] Optional stroke style
     * @param {number} [lineWidth=this.cellBorderWidth]     Optional line width
     */

    drawOutlineFlatHex(cell, style, lineWidth=this.cellBorderWidth) {
      if (lineWidth == 0)
        return;
      this.context.strokeStyle = style || this.strokeColors[cell.state] || this.defaultColor;
      this.context.lineWidth = lineWidth;
      this.drawPath(cell, this.flatVertices);
      this.context.stroke();
    }

    /**
     * Draw cell as filled cicle.
     *
     * An alternative drawing method that draws a filled circle instead of a hex. Must be manually added via
     * {@link CanvasAdapter#onDrawCell}.
     *
     * @param {Cell} cell                                  The cell being drawn
     * @param {string} [style=this.fillColors[cell.state]] Optional stroke style
     */

    drawFilledCircle(cell, style) {
      this.context.fillStyle = style || this.fillColors[cell.state] || this.defaultColor;
      this.drawCircle(cell);
      this.context.fill();
    }

    /**
     * Draw cell as outline cicle.
     *
     * An alternative drawing method that uses {@link CanvasAdapter#cellBorderWidth|this.cellBorderWidth} to draw an
     * outlined circle instead of a filled hex. Must be manually added via {@link CanvasAdapter#onDrawCell}.
     *
     * @param {Cell} cell                                  The cell being drawn
     * @param {string} [style=this.fillColors[cell.state]] Optional stroke style
     * @param {number} [lineWidth=this.cellBorderWidth]    Optional line width
     */

    drawOutlineCircle(cell, style, lineWidth=this.cellBorderWidth) {
      if (lineWidth == 0)
        return;
      this.context.strokeStyle = style || this.fillColors[cell.state] || this.defaultColor;
      this.context.lineWidth = lineWidth
      this.drawCircle(cell);
      this.context.stroke();
    }

    /**
     * Optional {@link CanvasAdapter#onDraw} callback that sorts model cells from lowest to highest state.
     *
     * This allows e.g. overlapping drawing functions to be executed in some sensible order, rather than the top-left
     * to-bottom-right order they would normally be drawn in. This has the potential to slow down larger models quite
     * a bit.
     */
    sortCellsAsc() {
      this.cells = this.cells || this.model.cells.slice();
      this.cells.sort((a, b) => a.state - b.state);
    }

    /**
     * Optional {@link CanvasAdapter#onDraw} callback that sorts model cells from highest to lowest state.
     */
    sortCellsDesc() {
      this.cells = this.cells || this.model.cells.slice();
      this.cells.sort((a, b) => b.state - a.state);
    }

    /**
     * Optional {@link CanvasAdapter#onDraw} callback that draws a solid background in the style given by
     *  {@link CanvasAdapter#backgroundColor|this.backgroundColor}.
     */
    drawBackground() {
      this.context.save();
      this.context.setTransform(1, 0, 0, 1, 0, 0);
      this.context.fillStyle = this.backgroundColor;
      this.context.fillRect(0, 0, this.context.canvas.width, this.context.canvas.height);
      this.context.restore();
    }

    /**
     * Optional {@link CanvasAdapter#onDraw} callback that draws a hexagonal background appropriate to the size if the
     * model in the style given by {@link CanvasAdapter#backgroundColor|this.backgroundColor}.
     */
    drawCubicBackground() {
      if (!this.model.radius) return;
      let radius = this.model.radius * this.cellRadius * APOTHEM * 2;
      this.context.beginPath();
      this.context.moveTo(radius, 0);
      for (let i = 0; i < 6; i++) {
        let a = Math.PI / 3 * i;
        this.context.lineTo(Math.cos(a) * radius, Math.sin(a) * radius);
      }
      this.context.closePath();
      this.context.fillStyle = this.backgroundColor;
      this.context.fill();
    }
  }

  // TODO: Add SVG adapter

  /**
   * A rule that returns the current state.
   *
   * @memberof Hexular.rules
   */
  function identityRule(cell) {
    return cell.state;
  }

  /**
   * A rule that returns 0.
   *
   * Debatably, this should return {@link Model#groundState}, but for various reasons doesn't.
   *
   * @memberof Hexular.rules
   */
  function nullRule(cell) {
    return 0;
  }

  // --- FILTERS ---

  /**
   * Constrain state to zero or one, with values less than zero being counted as the former.
   *
   * Must be run before {@link Hexular.filters.modFilter|modFilter}!.
   *
   * @memberof Hexular.filters
   */
  function binaryFilter(value) {
    return value > 0 ? 1 : 0;
  }

  /**
   * Add new state to current cell state if nonzero, otherwise set to zero.
   *
   * This is the some sense the opposite of, and generally &mdash; though not always &mdash; mutually exclusive with,
   * {@link Hexular.filters.binaryFilter|binaryFilter}.
   *
   * @memberof Hexular.filters
   */
  function deltaFilter(value, cell) {
    return value ? cell.state + value : 0;
  }

  /**
   * Prevent state from going below 0.
   *
   * Must be run before {@link Hexular.filters.modFilter|modFilter}!.
   *
   * @memberof Hexular.filters
   */
  function clipBottomFilter(value) {
    return Math.max(0, value);
  }

  /**
   * Prevent state from going above the value defined in {@link Model#numStates}.
   *
   * @memberof Hexular.filters
   */
  function clipTopFilter(value) {
    return Math.min(this.numStates - 1, value);
  }

  /**
   * Set new cell values to modulus with respect to {@link Model#numStates}.
   *
   * This filter has the effect of making states cyclical. Historically this was the default behavior. There is, in
   * principle, nothing preventing one from using non-numeric or complex multivalued cell states, but like much of the
   * boilerplate functionality, this filter is implemented on the assumption that states will be natural numbers.
   *
   * @memberof Hexular.filters
   */
  function modFilter(value) {
    return mod(value, this.numStates);
  }

  /**
   * Always set edge cells to ground state.
   *
   * This filter has the effect of disabling wraparound cells, since no cell state can affect a cell neighborhood
   * across the two-cell boundary width. This may have unexpected and undesirable effects with certain rules.
   *
   * @memberof Hexular.filters
   */
  function edgeFilter(value, cell) {
    return !cell.edge ? value : this.groundState;
  }

  // --- UTILITY FUNCTIONS ---

  /**
   * Given a cell with immediately-connected neighbors, find all cells out to a given radius, ordered by radial ring.
   *
   * This is used to order {@link CubicModel#cells}, and by the hex-drawing tools in Hexular Studio.
   *
   * @param {Cell} origin   Central cell
   * @param {number} radius A natural number greater than 0
   * @return {Cell[]}        An array of cells, including the origin, of length `3 * radius * (radius - 1) + 1`
   * @memberof Hexular.util
   * @see {@link Cell#nbrs}
   */
  function hexWrap(origin, radius) {
    let cells = [origin];
    for (let i = 1; i < radius; i++) {
      let cell = origin;
      // We select the first simple neighbor in the i-th ring
      for (let j = 0; j < i; j++)
        cell = cell.nbrs[1];

      for (let j = 0; j < 6; j++) {
        let dir = 1 + (j + 2) % 6;
        for (let k = 0; k < i; k++) {
          cell = cell.nbrs[dir];
          cells.push(cell);
        }
      }
    }
    return cells;
  }

  /**
  * Generates an elementary rule based on the state of a cell's neighbors plus optionally itself.
  *
  * The most significant (left-most) bit represents the lowest neighbor number in the selected range, while the least
  * significant bit represents the highest. Thus, the same rule masks can be used with `opts.range` set to either
  * `[0, 7]` or `[1, 7]` (default).
  *
  * Modeled roughly after Wolfram's
  * [Elementary Cellular Automaton]{@link http://mathworld.wolfram.com/ElementaryCellularAutomaton.html} rules.
  *
  * @param {BigInt|number[]} ruleDef          With default `opts.range`, 64-bit number indicating next position per
  *                                           possible state, or an array of 6-bit numbers indicating activation states
  *                                           numbers giving individual states where next cell state is 1
  * @param {object} opts                      Optional arguments
  * @param {number[]} [opts.range=[1, 7]]     Neighborhood range &mdash; default is N6 (immediate neighbors)
  * @param {number} [opts.miss=0]             Value to set on rule miss
  * @param {number} [opts.match=1]            Value to set on rule match
  * @param {boolean} [opts.missRel=false]     Whether to increment miss value from current state
  * @param {boolean} [opts.matchRel=false]    Whether to increment match value from current state
  * @param {boolean} [opts.rel=false]         If true, compare neighbors with current state, matching when former is
  *                                           nonzero and difference is >= 0
  * @return {function}                        A rule function taking a {@link Cell} instance and returning an integer
  * @memberof Hexular.util
  * @see {@link Cell#nbrs}
  **/
  function ruleBuilder(ruleDef, opts={}) {
    let defaults = {
      range: [1, 7],
      miss: 0,
      match: 1,
      missRel: 0,
      matchRel: 0,
      rel: 0,
    };
    let {range, miss, match, missRel, matchRel, rel} = Object.assign(defaults, opts);
    let [start, stop] = range;
    let rangeLength = stop - start;
    missRel = +missRel;
    matchRel = +matchRel;
    rel = +rel;

    if (typeof ruleDef == 'function' && ruleDef.n) {
      ruleDef = ruleDef.toObject()[0];
    }
    let n;
    if (ruleDef && ruleDef.length) {
      // For serialization consistency
      if (Array.isArray(ruleDef))
        ruleDef = ruleDef.slice().sort((a, b) => a - b);
      n = 0n;
      for (let state of ruleDef) {
        n = n | 1n << BigInt(state);
      }
    }
    else {
      n = BigInt(ruleDef);
    }

    let rule = (cell) => {
      let nbrStates = 0;
      for (let i = 0; i < rangeLength; i++) {
        let nbrState = cell.nbrs[start + i].state
        let nbrDiff = nbrState - cell.state * rel;
        nbrStates = nbrStates | ((nbrState && nbrDiff >= 0 ? 1 : 0) << (rangeLength - i - 1));
      }
      return (n >> BigInt(nbrStates)) % 2n ?
        (cell.state * matchRel) + match :
        (cell.state * missRel) + miss;
    };
    rule.n = n;
    rule.range = range;
    rule.toObject = () => [ruleDef, {range, miss, match, missRel, matchRel, rel}];
    rule.toString = () => JSON.stringify(rule.toObject());
    return rule;
  }

  // --- MATH STUFF ---

  /**
   * Modulo operation for reals.
   *
   * @param {number} a Dividend
   * @param {number} n Divisor
   * @return {number} Modulus
   * @memberof Hexular.math
   */
  function mod(a, n) {
    return ((a % n) + n) % n;
  }

  /**
   * Perform element-wise arithmetic operation on arbitrarily-dimensioned tensor.
   *
   * @param {number[]} obj    Arbitrary-dimensional array of numbers
   * @param {number} scalar   Real number
   * @param {string} [op='*'] Either '+' or '*' &mdash; for subtraction or division, invert `scalar` argument
   * @return {number[]}        Result array with same shape as `obj`
   * @memberof Hexular.math
   */
  function scalarOp(obj, scalar, op) {
    if (obj.map)
      return obj.map(function(val, i) {
        return scalarOp(val, scalar, op);
      });
    else
      return op == '+' ? obj + scalar : obj * scalar;
  }

  /**
   * Multiply row-major matrix by another matrix, or by a transposed one-dimensional vector (i.e. right-multiply a
   * matrix and a vector).
   *
   * @param {number[][]} a          Two-dimensional array representing an `m`*`n` matrix, where outer length = `m` and
   *                                 inner length = `n`
   * @param {number[][]|number[]} b Two-dimensional array representing an `p`*`q` matrix or a one-dimensional array
   *                                 representing `q`-length vector
   * @return {number[][]}            Two-dimensional array representing an `m`*`q` matrix
   * @memberof Hexular.math
   */
  function matrixMult(a, b) {
    let v = !Array.isArray(b[0])
    b = v ? [b] : b;
    let product = b.map((bCol) => {
      let productRow = [];
      for (let aRow = 0; aRow < a.length; aRow++) {
        productRow.push(bCol.reduce((acc, bEntry, aCol) => {
          return acc + a[aRow][aCol] * bEntry;
        }, 0));
      }
      return productRow;
    });
    return v ? product[0] : product;
  }

  /**
   * Element-wise addition of two identical-length arrays.
   *
   * @param {array} u `n`-dimensional first argument
   * @param {array} v `n`-dimensional second argument
   * @return {array}   `n`-dimensional sum
   * @memberof Hexular.math
   */
  function vectorAdd(u, v) {
    return Array.isArray(u) ? u.map((e, i) => add(e, v[i])) : u + v;
  }

  /**
   * Helper function for finding the maximum absolute value among several real numbers.
   *
   * @param {...number} ...args Real numbers
   * @return {number}            Maximum absolute value of provided arguments
   * @memberof Hexular.math
   */
  function absMax(...args) {
    return Math.max(...args.map((e) => Math.abs(e)));
  }

  /**
   * Convert [x, y] coordinates to cubic coordinates [u, v, w].
   *
   * @param {array} coord Tuple of coordinates [x, y]
   * @return {array}       Raw (real) cubic coordinates [u, v, w]
   * @memberof Hexular.math
   */
  function cartesianToCubic([x, y]) {
    let [v, u] = matrixMult(math.invBasis, [x, y]);
    let w = -u - v;
    return [u, v, w];
  }

  /**
   * Convert real-valued [u, v, w] to their rounded, whole-number counterparts.
   *
   * @param {array} coord       Array of real-valued cubic coordinates [u, v, w]
   * @param {number} [radius=1] Optional radius scalar &mdash; for converting "pixel" coords to "cell" coords
   * @return {array}             Integer cubic coordinates [u, v, w]
   * @memberof Hexular.math
   */
  function roundCubic([u, v, w], radius = 1) {
    [u, v, w] = scalarOp([u, v, w], 1 / radius);
    let ru = Math.round(u);
    let rv = Math.round(v);
    let rw = Math.round(w);

    let du = Math.abs(ru - u);
    let dv = Math.abs(rv - v);
    let dw = Math.abs(rw - w);

    if (du > dv && du > dw)
      ru = -rv - rw;
    else if (du > dw)
      rv = -ru - rw;
    else
      rw = -ru - rv;
    return [ru, rv, rw];
  }

  // ---

  let attributes = {
    DEFAULTS: Object.assign(DEFAULTS, {model: CubicModel}),
    enums: {
      TYPE_FLAT,
      TYPE_POINTY,
    },
    rules: {
      identityRule,
      nullRule,
    },
    filters: {
      binaryFilter,
      deltaFilter,
      clipBottomFilter,
      clipTopFilter,
      modFilter,
      edgeFilter,
    },
    util: {
      hexWrap,
      ruleBuilder,
    },
    math: Object.assign(math, {
      mod,
      scalarOp,
      matrixMult,
      vectorAdd,
      absMax,
      cartesianToCubic,
      roundCubic,
      flatVertices: math.vertices,
      pointyVertices: math.vertices.map(([x, y]) => [y, x]),
    }),
    classes: {
      adapters: {
        CanvasAdapter,
      },
      models: {
        OffsetModel,
        CubicModel,
      },
      HexError,
      Model,
      Cell,
      HookList,
      Adapter,
    },
  };

  /**
   * Principal function object assigned to global `Hexular` object or returned as module.
   *
   * @param {...object} ...args Arguments to pass to Model constructor
   * @return {Model}             Model instance
   * @global
   */
  const Hexular = (...args) => {
    let Class = (args[0] && args[0].prototype instanceof Model) ? args.shift() : attributes.DEFAULTS.model;
    return new Class(...args);
  }

  Object.assign(Hexular, attributes);

  return Hexular;
})();

if (typeof module != 'undefined')
  module.exports = Hexular;

class Action {
  constructor(board, ...args) {
    let config = board.config;
    let model = board.model;
    Object.assign(this, {board, config, model}, ...args);
    this.model = board.model;
    this.coords = [];
    this.board.fgAdapter.clear();
  }

  start() {}
  move() {}
  end() {}

  _setCells(...cells) {
    for (let cell of cells) {
      if (this.board.fgAdapter.stateBuffer.get(cell) == this.setState)
        continue;
      this.board.fgAdapter.stateBuffer.set(cell, this.setState)
      this.board.fgAdapter.defaultDrawBuffer(cell);
    }
  }

  _selectWithSize(arg) {
    if (Array.isArray(arg))
      return [].concat(...arg.map((e) => this._selectWithSize(e)));
    return arg ? Hexular.util.hexWrap(arg, this.config.toolSize) : [];
  }

  _applyBuffer() {
    if (this.board.fgAdapter.stateBuffer.size > 0) {
      this.board.newHistoryState();
      this.board.fgAdapter.stateBuffer.forEach((state, cell) => {
        cell.state = state;
      });
      this.board.fgAdapter.stateBuffer.clear();
      this.board.fgAdapter.clear();
      this.board.draw();
    }
  }

  _getCoord(pointerEv) {
    return [pointerEv.pageX, pointerEv.pageY];
  }

  _getPointerCoord(ev) {
    let x, y;
    if (ev.pageX)
      [x, y] = this._getCoord(ev);
    else if (ev.touches && ev.touches[0])
      [x, y] = this._getCoord(ev.touches[0]);
    return [x, y];
  }

  _getAllCoords(ev) {
    if (ev.pageX)
      return [this._getCoord(ev)];
    else if (ev.touches)
      return Array.from(ev.touches).map((e) => this._getCoord(e));
  }

  _getHypot(a, b) {
    return Math.hypot(b[0] - a[0], b[1] - a[1]);
  }

  _hypotToModel(h) {
    return h / this.model.cellApothem / this.board.scale;
  }
}


class MoveAction extends Action {
  start(ev) {
    this.startEv = ev;
    this.coords = [this._getPointerCoord(ev)];
  }
  move(ev) {
    this.coords.push(this._getPointerCoord(ev));
    let [last, cur] = this.coords.slice(-2);
    let diffX = cur[0] - last[0];
    let diffY = cur[1] - last[1];
    this.board.translate([diffX, diffY]);
  }
}

class PinchAction extends Action {
  start(ev) {
    this.hypot = this._getHypotFromTouch(ev);
  }
  move(ev) {
    let newHypot = this._getHypotFromTouch(ev);
    this.board.scaleRelative(newHypot / this.hypot);
    this.hypot = newHypot;
  }
  _getHypotFromTouch(ev) {
    let t0 = this._getCoord(ev.touches[0]);
    let t1 = this._getCoord(ev.touches[1]);
    return this._getHypot(t0, t1);
  }
}

class PaintAction extends Action {
  constructor(...args) {
    super(...args);
    if (this.setState == null)
      this.setState = this.config.getPaintColor(0);
    if (this.ctrl)
      this.setState = this.model.groundState;
  }

  end() {
    this._applyBuffer();
    this.board.storeModelState();
  }
}

class FillAction extends PaintAction {
  start() {
    let homeCell = this.board.selected;
    let fillState = homeCell.state;
    let cellSet = new Set();
    cellSet.add(homeCell);
    let queue = homeCell.nbrs.slice(1, 7);
    let cur;
    while (cur = queue.shift()) {
      if (cur.state != fillState || cellSet.has(cur))
        continue;
      cellSet.add(cur);
      for (let i = 1; i < 7; i++)
        queue.push(cur.nbrs[i]);
    }
    let cells = Array.from(cellSet);
    this._setCells(...cells);
  }
}

class BrushAction extends PaintAction {
  start() {
    this._setCells(...this._selectWithSize(this.board.selected));
  }

  move() {
    this._setCells(...this._selectWithSize(this.board.selected));
  }
}

class LineAction extends PaintAction {
  start(ev) {
    this.originCell = this.board.selected;
    this.a = this.board.modelToWindow(this.model.getCoord(this.originCell));
    this.move(ev);
  }

  move(ev) {
    this.b = this._getPointerCoord(ev);
    this.length = this._getHypot(this.a, this.b);
    this.info = Math.round(this._hypotToModel(this.length) / 2);
    this._calculateCells();
  }

  _calculateCells() {
    let samples = this._hypotToModel(this.length);
    let [x, y] = this.a.slice();
    let xSample = (this.b[0] - this.a[0]) / samples;
    let ySample = (this.b[1] - this.a[1]) / samples;
    let cells = this._selectWithSize(this.originCell);
    for (let i = 1; i < samples; i++) {
      x += xSample;
      y += ySample;
      let cell = this.board.cellAt([x, y]);
      // We don't actually care about dups tho this probably could be tightened up a bit
      cells = cells.concat(this._selectWithSize(cell));
    }
    this._bufferCells(cells);
  }

  _bufferCells(cells) {
    this.board.fgAdapter.clear();
    this.board.fgAdapter.stateBuffer.clear();
    cells.forEach((cell) => {
      this._setCells(cell);
    })
  }
}

class LocklineAction extends LineAction {
  move(ev) {
    this.b = this._getPointerCoord(ev);
    let x = this.b[0] - this.a[0];
    let y = this.b[1] - this.a[1];
    let h = this.length = this._getHypot(this.a, this.b);
    let a = Math.acos(x / h) / Hexular.math.hextant;
    if (Math.sin(y / h) < 0)
      a = 6 - a;
    let aRound = Math.round(a) % 6;
    let xRound = Math.cos(aRound * Hexular.math.hextant) * h;
    let yRound = Math.sin(aRound * Hexular.math.hextant) * h;
    this.b = [this.a[0] + xRound, this.a[1] + yRound];
    this.info = Math.round(this._hypotToModel(this.length) / 2);
    this._calculateCells();
  }
}

class HexAction extends LineAction {
  _calculateCells() {
    let pixRad = this.length / this.board.scale;
    this.radius = Math.ceil(pixRad / (this.board.model.cellApothem * 2) + 0.5);
    let cells = Hexular.util.hexWrap(this.originCell, this.radius);
    let outline = cells.slice((-this.radius + 1) * 6);
    let expandedOutline = this._selectWithSize(outline);
    this._hexToBuffer(cells, expandedOutline);
  }
}

class HexFilledAction extends HexAction {
  _hexToBuffer(cells, expandedOutline) {
    this._bufferCells(cells.concat(expandedOutline));
  }
}

class HexOutlineAction extends HexAction {
  _hexToBuffer(cells, expandedOutline) {
    this._bufferCells(expandedOutline);
  }
}
class Board {
  static resize(opts={}) {
    return new Promise((resolve, reject) => {
      document.body.classList.add('splash');
      let oldBoard = Board.instance;
      oldBoard && oldBoard.stop();
     setTimeout(async () => {
        let board = new Board(opts);
        Board.instance = board;
        if (oldBoard) {
          board.undoStack = oldBoard.undoStack;
          board.redoStack = oldBoard.redoStack;
          board.bgAdapter.onDraw.replace(oldBoard.bgAdapter.onDraw);
          board.bgAdapter.onDrawCell.replace(oldBoard.bgAdapter.onDrawCell);
          board.refreshHistoryButtons();
        }
        Board.config = board.config;
        Board.model = board.model;
        Board.bgAdapter = board.bgAdapter;
        Board.fgAdapter = board.fgAdapter;
        Board.modals = board.modals;
        await board.draw();
        document.body.classList.remove('splash');
        resolve();
      }, 50);
    });
  }

  static get aspectRatio() {
    return window.innerWidth / window.innerHeight;
  }

  constructor(...args) {
    let props = {
      selected: null,
      lastSet: null,
      setState: null,
      timer: null,
      messageTimer: null,
      undoStack: [],
      redoStack: [],
      msgIdx: 0,
      shift: false,
      configMenu: false,
      imageCapture: null,
      hooks: {
        incrementStep: [],
        playStep: [],
        step: [],
        timer: [],
      },
      scaling: false,
      scaleQueue: [],
      toolClasses: {
        move: MoveAction,
        fill: FillAction,
        brush: BrushAction,
        line: LineAction,
        lockline: LocklineAction,
        hexfilled: HexFilledAction,
        hexoutline: HexOutlineAction,
        pinch: PinchAction,
      },
      sizableTools: [
        'brush',
        'line',
        'lockline',
        'hexfilled',
        'hexoutline',
      ],
      modal: null,
      modalTranslate: null,
      container: document.querySelector('.container'),
      overlay: document.querySelector('.overlay'),
      message: document.querySelector('.message'),
      menus: {
        color: document.querySelector('#color-menu'),
        config: document.querySelector('#config-menu'),
      },
      infoBoxes: {
        cursor: document.querySelector('.info-cursor'),
        timer: document.querySelector('.info-timer'),
        steps: document.querySelector('.info-steps'),
        tool: document.querySelector('.info-tool'),
      },
      buttons: {
        toolHider: document.querySelector('.tool-hider button'),
        toggleRecord: document.querySelector('#toggle-record'),
        togglePlay: document.querySelector('#toggle-play'),
        step: document.querySelector('#step'),
        clear: document.querySelector('#clear'),
        undo: document.querySelector('#undo'),
        redo: document.querySelector('#redo'),
        toggleMenu: document.querySelector('#toggle-menu'),
        showConfig: document.querySelector('#show-config'),
        showRb: document.querySelector('#show-rb'),
        showResize: document.querySelector('#show-resize'),
        showCustom: document.querySelector('#show-custom'),
        showClear: document.querySelector('#show-clear'),
        saveSnapshot: document.querySelector('#snapshot-save'),
        loadSnapshot: document.querySelector('#snapshot-load'),
        showDoc: document.querySelector('#show-doc'),
        saveImage: document.querySelector('#save-image'),
        toggleImageCapture: document.querySelector('#toggle-image-capture'),
        load: document.querySelector('#load'),
        save: document.querySelector('#save'),
        saveData: document.querySelector('#save-data'),
        loadData: document.querySelector('#load-data'),
      },
      tools: {
        fill: document.querySelector('#tool-fill'),
        move: document.querySelector('#tool-move'),
        brush: document.querySelector('#tool-brush'),
        line: document.querySelector('#tool-line'),
        lockline: document.querySelector('#tool-lockline'),
        hexfilled: document.querySelector('#tool-hexfilled'),
        hexoutline: document.querySelector('#tool-hexoutline'),
      },
      toolSizes: [
        document.querySelector('#ts-1'),
        document.querySelector('#ts-2'),
        document.querySelector('#ts-3'),
      ],
      toolMisc: {
        center: document.querySelector('#center'),
        color: document.querySelector('#tool-color'),
      },
      colorButtons: Array.from(document.querySelectorAll('.toolbar.colors button')),
    };
    props.disableWhenRecording = [
      props.buttons.step,
      props.buttons.undo,
      props.buttons.redo,
      props.buttons.saveSnapshot,
      props.buttons.loadSnapshot,
      props.buttons.saveImage,
      props.buttons.toggleImageCapture,
      props.buttons.save,
      props.buttons.load,
    ];
    Object.assign(this, props);
    this.config = new Config(this, ...args);

    // Initialize canvases
    this.bg = document.createElement('canvas');
    this.fg = document.createElement('canvas');
    this.bg.classList.add('canvas', 'canvas-bg');
    this.fg.classList.add('canvas', 'canvas-fg');
    this.bgCtx = this.bg.getContext('2d');
    this.fgCtx = this.fg.getContext('2d');

    while (this.container.firstChild)
      this.container.firstChild.remove();
    this.container.appendChild(this.bg);
    this.container.appendChild(this.fg);

    window.onblur = (ev) => this.handleBlur(ev);
    window.onkeydown = (ev) => this.handleKey(ev);
    window.onkeyup = (ev) => this.handleKey(ev);
    window.oncontextmenu = (ev) => this.handleContextmenu(ev);
    window.onresize = (ev) => this.resize();
    window.onwheel = (ev) => this.handleScale(ev);
    OnMouseEvent(this, this.handleMouse);
    OnTouchEvent(this, this.handleTouch);

    this.buttons.toolHider.onclick = this.click(this.toggleToolHidden);
    this.buttons.togglePlay.onclick = this.click(this.togglePlay);
    this.buttons.step.onclick = this.click(this.step);
    this.buttons.clear.onclick = this.click(this.clear);
    this.buttons.undo.onclick = this.click(this.undo);
    this.buttons.redo.onclick = this.click(this.redo);
    this.buttons.toggleRecord.onclick = this.click(this.toggleRecord);
    this.buttons.toggleMenu.onclick = this.click(this.toggleMenu);
    this.buttons.showConfig.onmousedown = () => this.toggleModal('config');
    this.buttons.showResize.onmousedown = () => this.toggleModal('resize');
    this.buttons.showRb.onmousedown = () => this.toggleModal('rb');
    this.buttons.showCustom.onmousedown = () => this.toggleModal('custom');
    this.buttons.showClear.onmousedown = () => this.handleClearStorage();

    this.buttons.saveSnapshot.onclick = this.click(this.saveSnapshot);
    this.buttons.loadSnapshot.onclick = this.click(this.loadSnapshot);
    this.buttons.showDoc.onclick = this.click(this.showDoc);
    this.buttons.saveImage.onclick = this.click(this.promptSaveImage);
    this.buttons.toggleImageCapture.onclick = this.click(this.toggleImageCapture);
    this.buttons.load.onclick = this.click(this.load);
    this.buttons.save.onclick = this.click(this.save);
    this.buttons.loadData.onclick = this.click(this.loadData);
    this.buttons.saveData.onclick = this.click(this.saveData);

    this.tools.move.onclick = this.click((ev) => this.config.setTool('move'), this.config);
    this.tools.brush.onclick = this.click((ev) => this.config.setTool('brush'), this.config);
    this.tools.brush.onclick = this.click((ev) => this.config.setTool('brush'), this.config);
    this.tools.line.onclick = this.click((ev) => this.config.setTool('line'), this.config);
    this.tools.lockline.onclick = this.click((ev) => this.config.setTool('lockline'), this.config);
    this.tools.hexfilled.onclick = this.click((ev) => this.config.setTool('hexfilled'), this.config);
    this.tools.hexoutline.onclick = this.click((ev) => this.config.setTool('hexoutline'), this.config);
    this.toolMisc.center.onclick = this.click(this.resize);
    this.toolMisc.color.onclick = this.click(this.config.setPaintColorMode, this.config);
    this.toolSizes.forEach((button, i) => {
      button.onclick = this.click(() => this.config.setToolSize(i + 1), this.config);
    });
    this.colorButtons.forEach((button, i) => {
      button.onmousedown = (ev) => this.handleSetColor(ev, i);
    });

    let {radius, numStates, groundState, cellRadius, cellGap, colors} = this.config;
    this.model = Hexular({radius, numStates, groundState, cellRadius});
    this.bgAdapter = this.model.CanvasAdapter({context: this.bgCtx, cellGap, colors});
    this.fgAdapter = this.model.CanvasAdapter({context: this.fgCtx, cellGap, colors});
    this.resize();

    this.modals = {
      confirm: new ConfirmModal(this, 'confirm'),
      config: new ConfigModal(this, 'config'),
      custom: new CustomModal(this, 'custom'),
      rb: new RbModal(this, 'rb'),
      resize: new ResizeModal(this, 'resize'),
    }
    this.toggleModal();
    this.config.restoreModel();
    this.config.initialize();
  }

  get running() { return !!this.timer; }

  // Bypass Firefox's idiotic space-click
  click(fn, bind=this) {
    return (ev) => ev.pageX && ev.pageY && fn.bind(bind)();
  }

  eachContext(fn) {
    [this.bgCtx, this.fgCtx].forEach(fn);
  }

  draw() {
    if (!this.drawPromise && this.bgAdapter) {
      this.drawPromise = new Promise((resolve, reject) => {
        if (!this.running) {
          requestAnimationFrame(() => {
            try {
              this.drawPromise && this.drawSync();
              resolve();
            }
            catch (e) {
              reject(e);
            }
          });
        }
        else {
          resolve();
        }
      });
    }
    return this.drawPromise;
  }

  drawSync() {
    this.bgAdapter.draw();
    this.recorder && this.recorder.draw();
    this.drawPromise = null;
  }

  // Button handlers (can also be called directly)

  toggleRecord() {
    if (!this.recorder) {
      if (!this.running)
        requestAnimationFrame(() => this.start());
      this.playStart = Date.now();
      this.disableWhenRecording.forEach((e) => e.disabled = true);
      this.buttons.toggleRecord.className = 'icon-stop active';
      this.setButtonTitle(this.buttons.toggleRecord, 'Stop');
      this.recorder = new Recorder(this);
      this.config.setRecordingMode(true);
      this.draw().then(() => this.recorder.start());
    }
    else {
      this.recorder.stop();
      this.recorder = null;
      requestAnimationFrame(() => {
        this.stop();
        this.draw();
      });
      if (!this.imageCapture)
        this.config.setRecordingMode(false);
      this.buttons.toggleRecord.className = 'icon-record';
      this.setButtonTitle(this.buttons.toggleRecord, 'Record');
      this.disableWhenRecording.forEach((e) => e.disabled = false);
    }
  }

  togglePlay() {
    if (!this.running) {
      this.start();
    }
    else {
      this.stop();
    }
  }

  start() {
    if (!this.running) {
      this.playStart = this.playStart || Date.now();
      this.timer = setInterval(this.step.bind(this), this.config.interval);
      this.startMeta();
      this.buttons.step.disabled = true;
      this.buttons.togglePlay.className = 'icon-pause';
      this.setButtonTitle(this.buttons.togglePlay, 'Pause');
    }
  }

  stop() {
    if (this.running) {
      if (this.recorder)
        this.toggleRecord();
      clearInterval(this.timer);
      this.timer = null;
      this.playStart = null;
      this.stopMeta();
      this.buttons.step.disabled = false;
      this.buttons.togglePlay.className = 'icon-play';
      this.setButtonTitle(this.buttons.togglePlay, 'Play');
    }
  }

  startMeta() {
    let hooks = this.hooks.timer.slice();
    let sexFmt = (i) => ('00' + i).slice(-2);
    this.infoBoxes.cursor.classList.add('hidden');
    this.recorder && this.infoBoxes.timer.classList.add('recording');
    this.metaInterval = setInterval(() => {
      let deltaMs = Date.now() - this.playStart;
      let deltaSecs = Math.floor(deltaMs / 1000);
      let thirds = Math.floor((deltaMs % 1000) * 60 / 1000);
      let secs = deltaSecs % 60;
      let mins = Math.floor(deltaSecs / 60) % 60;
      let str = `${sexFmt(mins)}:${sexFmt(secs)}:${sexFmt(thirds)}`;
      this.setInfoBox('timer', str);
      while (hooks[0] && hooks[0].trigger <= deltaMs) {
        let hook = hooks.shift();
        hook.run();
      }
    }, 50);
  }

  stopMeta() {
    clearInterval(this.metaInterval);
    this.metaInterval = null;
    this.setInfoBox('timer');
    this.infoBoxes.cursor.classList.remove('hidden');
    this.infoBoxes.timer.classList.remove('recording');
  }

  async step() {
    this.newHistoryState();
    try {
      this.model.step();
      this.drawSync();
      this.storeModelState();
      if (!this.model.changed && this.config.autopause) {
        this.stop();
        this.undo(true);
      }
      else {
        this.config.setSteps(this.config.steps + 1);
        this.running
          ? this.hooks.playStep.forEach((e) => e.run())
          : this.hooks.incrementStep.forEach((e) => e.run());
        this.hooks.step.forEach((e) => e.run());
      }
    }
    catch (e) {
      console.error(e);
      this.setMessage(e, 'error');
      if (this.running)
        this.stop();
    }
  }

  clear() {
    this.newHistoryState();
    this.model.clear();
    this.draw();
    this.storeModelState();
    this.config.setSteps(0);
  }

  addHook(...args) {
    let [key, trigger, run] = args.length == 3 ? args : [args[0], null, args[1]];
    if (this.hooks[key]) {
      this.hooks[key].push({trigger, run});
      this.hooks[key].sort((a, b) => a.trigger - b.trigger);
    }
  }

  clearHooks(key) {
    if (this.hooks[key])
      this.hooks[key] = [];
  }

  toggleMenu(state=!this.configMenu) {
    this.configMenu = state;
    if (state) {
      this.buttons.toggleMenu.classList.add('active');
      this.menus.config.classList.remove('hidden')
    }
    else {
      this.buttons.toggleMenu.classList.remove('active');
      this.menus.config.classList.add('hidden')
    }
  }

  toggleModal(modal) {
    let selected = this.modals[modal];
    let current = this.modal;
    Object.values(this.modals).forEach((e) => e.close());
    if (selected && current != selected) {
      this.toggleMenu(false);
      this.modals[modal].open();
    }
    else if (!selected) {
      this.fg.focus();
    }
  }

  translateModal(coord) {
    if (!this.modalTranslate) {
      this.modalTranslate = coord;
    }
    else if (coord && this.modal) {
      let left = parseInt(this.modal.modal.style.left || 0);
      let top = parseInt(this.modal.modal.style.top || 0);
      this.modal.modal.style.left = `${left + coord[0] - this.modalTranslate[0]}px`;
      this.modal.modal.style.top = `${top + coord[1] - this.modalTranslate[1]}px`;
      this.modalTranslate = coord;
    }
    else {
      this.modalTranslate = null;
    }
  }

  showDoc() {
    window.open('doc/', '_blank');
  }

  toggleToolHidden() {
    let hidden = document.body.classList.toggle('tool-hidden');
    this.buttons.toolHider.classList.toggle('active');
    this.buttons.toolHider.classList.toggle('icon-eye');
    this.buttons.toolHider.classList.toggle('icon-eye-off');
    setTimeout(() => this.resizeMenu(), 500);
  }

  setButtonTitle(button, title) {
    let cur = button.title.split(' ');
    cur[0] = title;
    button.title = cur.join(' ');
  }

  setCursorInfoInfo() {
    let cell = this.selected;
    let coord = cell && cell.coord.map((c) => (c > 0 ? '+' : '-') + ('0' + Math.abs(c)).slice(-2));
    this.setInfoBox('cursor', coord);
  }

  setToolInfo() {
    let info = this.action && this.action.info;
    this.setInfoBox('tool', info);
  }

  // Save/load

  saveSnapshot() {
    this.config.storeModel('modelSnapshot', this.model.export());
    this.setMessage('Snapshot saved!');
  }

  loadSnapshot() {
    this.newHistoryState();
    let bytes = this.config.loadModel('modelSnapshot');
    if (bytes) {
      let cur = this.model.export();
      let diff = false;
      for (let i = 0; i < cur.length; i++)
        if (cur[i] != bytes[i]) {
          diff = true;
          break;
        }
      if (diff) {
        this.model.import(bytes);
        this.draw();
        this.storeModelState();
        this.setMessage('Snapshot loaded!');
      }
      else {
        this.setMessage('Snapshot already loaded!', 'warning');
      }
    }
    else {
      this.setMessage('No snapshot found!', 'warning');
    }
  }

  toggleImageCapture() {
    if (!this.imageCapture) {
      this.imageCapture = [];
      this.config.setRecordingMode(true);
      this.draw();
      let fn = async (e) => {
        this.imageCapture.push([this.getImageFilename(), await this.saveImage()]);
      };
      fn.imageCaptureCb = true;
      this.addHook('step', fn);
      // Capture current state
      fn();
      this.buttons.toggleImageCapture.classList.add('active');
    }
    else {
      if (!this.recorder)
        this.config.setRecordingMode(false);
      this.draw();
      this.processImageCaptures(this.imageCapture);
      this.imageCapture = null;
      this.hooks.step = this.hooks.step.filter((e) => !e.run.imageCaptureCb);
      this.buttons.toggleImageCapture.classList.remove('active');
    }
  }

  async processImageCaptures(captures) {
    if (captures.length < 0)
      return;
    let string2bytes = (str) => Uint8Array.from(str.split('').map((e) => e.charCodeAt(0)));
    let padString = (str, length) => (str + ' '.repeat(length)).slice(0, length);
    let segments = [string2bytes('!<arch>\n')];
    captures.forEach(([filename, dataUri]) => {
      // I have literally no idea what I'm doing
      let data = atob(dataUri.split(',')[1]);
      let length = data.length;
      if (data.length % 2 == 1) {
        data += '\n';
      }
      let bytes = Uint8Array.from(string2bytes(data));
      let header = padString(filename + '/', 16)
        + padString('0', 12)
        + padString('0', 6)
        + padString('0', 6)
        + padString('644', 8)
        + padString(bytes.length.toString(), 10)
        + '`\n';
      segments.push(string2bytes(header));
      segments.push(bytes);
    });
    let blob = new Blob(segments, {type: 'application/x-archive'});
    let dataUri = window.URL.createObjectURL(blob);
    this.promptDownload(this.config.defaultArchiveFilename, dataUri);
  }

  async saveImage() {
    let recordingMode = this.config.recordingMode;
    this.config.setRecordingMode(true);
    await this.draw();
    let transferCanvas = new TransferCanvas(this);
    let dataUri = transferCanvas.canvas.toDataURL('image/png');
    this.config.setRecordingMode(recordingMode);
    await this.draw();
    return transferCanvas.canvas.toDataURL('image/png');
  }

  async promptSaveImage() {
    let dataUri = await this.saveImage();
    this.promptDownload(this.getImageFilename(), dataUri);
  }

  getImageFilename() {
    let padStep = ('000' + this.config.steps).slice(-3);
    return `${this.config.defaultImageFilenameBase}-${padStep}.png`;
  }

  save() {
    let bytes = this.model.export();
    let blob = new Blob([bytes], {type: 'application/octet-stream'});
    let dataUri = window.URL.createObjectURL(blob);
    this.promptDownload(this.config.defaultFilename, dataUri);
  }

  load() {
    let Class = window[this.config.arrayType];
    this.newHistoryState();
    let fileLoader = new FileLoader('.bin', {reader: 'readAsArrayBuffer'});
    fileLoader.onload = (result) => {
      let bytes = new Class(result);
      this.model.import(bytes);
      this.draw();
      this.storeModelState();
      this.setMessage('Model loaded!');
    };
    fileLoader.prompt();
  }

  saveData() {
    this.config.storeLocalConfig();
    this.config.storeSessionConfig();
    let obj = this.config.retrieveConfig();
    let dataUri = `data:application/json,${encodeURIComponent(JSON.stringify(obj, null, 2))}`;
    this.promptDownload(this.config.defaultSettingsFilename, dataUri);
  }

  loadData() {
    let fileLoader = new FileLoader('.json');
    fileLoader.onload = (result) => {
      try {
        let config = JSON.parse(result);
        this.config.restoreState(config);
        this.config.initialize();
        this.config.storeLocalConfigAsync();
        this.config.storeSessionConfigAsync();
        this.config.setTheme();
        this.config.setCellGap();
        this.config.setCellBorderWidth();
        this.setMessage('Settings restored!');
      }
      catch (e) {
        this.setMessage('Unable to parse settings file!', 'error');
        console.error(e);
      }
    };
    fileLoader.prompt();
  }

  import() {
    let fileLoader = new FileLoader('.js', {multiple: true});
    fileLoader.onload =  (code) => {
      try {
        eval(code) // lol
        this.modals.config.update();
        this.setMessage('Custom code imorted!');
      }
      catch (e) {
        this.setMessage(e.toString(), 'error');
      }

    };
    fileLoader.filter = (files) => {
      let result = files.map((file) => file.type.indexOf('javascript') >= 0);
      result.some((e) => !e) && this.setMessage('Not all selected files are JavaScript files', 'error');
      return result;
    };
    fileLoader.prompt();
  }

  promptDownload(filename, dataUri) {
    let a = document.createElement('a');
    a.href = dataUri;
    a.download = filename;
    a.click();
  }

 // Undo/redo stuff

  newHistoryState() {
    let state = this.model.export();
    state.steps = this.config.steps;
    this.undoStack.push(state);
    if (this.undoStack.length > this.config.undoStackSize)
      this.undoStack.shift();
    this.redoStack = [];
    this.refreshHistoryButtons();
  }

  storeModelState(bytes) {
    bytes = bytes || this.model.export();
    this.config.storeModel('modelState', bytes);
  }

  undo(discard=false) {
    if (this.recorder) return;
    let nextState = this.undoStack.pop();
    if (nextState) {
      let curState = this.model.export()
      curState.steps = this.config.steps;
      this.model.import(nextState);
      this.storeModelState(nextState);
      this.config.setSteps(nextState.steps);
      if (!discard)
        this.redoStack.push(curState);
      this.draw();
      this.refreshHistoryButtons();
    }
  }

  redo(discard=false) {
    if (this.recorder) return;
    let nextState = this.redoStack.pop();
    if (nextState) {
      let curState = this.model.export()
      curState.steps = this.config.steps;
      this.model.import(nextState);
      this.storeModelState(nextState);
      this.config.setSteps(nextState.steps);
      if (!discard)
        this.undoStack.push(curState);
      this.draw();
      this.refreshHistoryButtons();
    }
  }

  resize() {
    this.resizeMenu();

    // Canvas stuff
    this.canvasWidth = this.config.logicalWidth / this.config.scaleFactor;
    this.canvasHeight = this.config.logicalHeight / this.config.scaleFactor;
    this.translateX = 0;
    this.translateY = 0;
    this.scaleX = 1;
    this.scaleY = 1;
    this.offsetX = 0;
    this.offsetY = 0;
    this.scale = 1
    this.eachContext((ctx) => {
      ctx.canvas.width = this.canvasWidth;
      ctx.canvas.height = this.canvasHeight;
      ctx.setTransform(1, 0, 0, 1, 0, 0);
    });
    this.scaleTo(this.config.defaultScale);
    // Resize
    let [oldX, oldY] = [this.scaleX, this.scaleY];
    this.scaleX = this.canvasWidth / window.innerWidth;
    this.scaleY = this.canvasHeight / window.innerHeight;
    let [scaleX, scaleY] = [this.scaleX / oldX, this.scaleY / oldY];
    this.eachContext((ctx) => {
      ctx.scale(scaleX, scaleY);
    });
    // Translate to center
    this.translate([this.canvasWidth / this.scaleX / 2, this.canvasHeight / this.scaleY / 2]);
    this.draw();
  }

  resizeMenu() {
    let {x, y, height} = this.buttons.toggleMenu.getBoundingClientRect();
    this.menus.config.style.top = `${y + height}px`;
    this.menus.config.style.left = `${x}px`;
  }

  refreshHistoryButtons() {
    this.buttons.undo.disabled = +!this.undoStack.length || this.recorder;
    this.buttons.redo.disabled = +!this.redoStack.length;
  }

  scaleRelative(scale) {
    this.scale *= scale;
    this.eachContext((ctx) => {
      ctx.scale(scale, scale);
    });
    this.draw();
    this.drawSelectedCell();
  }

  scaleTo(target, interval=0, step=50, timingFn) {
    if (this.scaling) {
      this.scaleQueue.push([target, interval, step, timingFn]);
      return;
    } else if (!interval) {
      this.scaleRelative(target / this.scale);
      return;
    }
    this.scaling = true;
    let diff = target - this.scale;
    let numSteps = Math.ceil(interval / step);
    timingFn = timingFn || this.defaultTimingFn;
    let steps = Array(numSteps).fill(null).map((_, idx) => timingFn(idx, numSteps));
    steps = steps.map((e, i) => (steps[i + 1] || 1) - e);
    let fn = (increment) => {
      if (steps.length) {
        let stepTarget = this.scale + diff * increment;
        this.scaleRelative(stepTarget / this.scale);
        setTimeout(() => fn(steps.pop()), step);
      }
      else {
        this.scaleRelative(target / this.scale);
        this.scaling = false;
        if (this.scaleQueue.length) {
          this.scaleTo(...this.scaleQueue.shift());
        }
      }
    };
    setTimeout(() => fn(steps.pop()), step);
  }

  setInfoBox(boxName, value) {
    value = value != null ? value.toString() : '';
    let box = this.infoBoxes[boxName];
    let lastValue = box.innerHTML;
    box.innerHTML = value;
    if (lastValue == '' && value != '')
      box.classList.add('active');
    else if (lastValue != '' && value == '')
      box.classList.remove('active');
  }

  translate([x, y]) {
    this.translateX += x;
    this.translateY += y;
    x /= this.scale;
    y /= this.scale;
    this.eachContext((ctx) => {
      ctx.translate(x, y);
    });
    this.draw();
  }

  // Page/canvas listeners

  handleClearStorage() {
    this.modals.confirm.ask('Are you sure you want to clear local data, including custom rules and presets?')
    .then((e) => {
      if (e) {
        this.config.clearStorage();
        Board.resize(this.config.radius);
        Board.instance.setMessage('Settings cleared!');
      }
    }).catch((e) => { throw e; });
  }

  handleSetColor(ev, color) {
    if (ev.buttons & 1)
      this.config.setPaintColor(0, color);
    if (ev.buttons & 2)
      this.config.setPaintColor(1, color);
  }

  handleBlur(ev) {
    this.shift = false;
    this.config.setTool('move');
  }

  handleScale(ev) {
    let scale = 1 - Math.sign(ev.deltaY) * 0.1;
    this.scaleRelative(scale);
    this.draw();
  }

  handleContextmenu(ev) {
    if (ev.target == this.fg || this.colorButtons.includes(ev.target))
      ev.preventDefault();
  }

  handleKey(ev) {
    let key = ev.key.toLowerCase();

    // Modal-specific stuff
    if (this.modal && ev.type == 'keydown') {
      let isInput = ['TEXTAREA', 'INPUT'].includes(ev.target.tagName);
      if (ev.key == 'Escape') {
        this.toggleModal();
        ev.preventDefault();
        return;
      }
      else if (!ev.repeat && ev.ctrlKey) {
        if (key == 'a') {
          if (isInput) {
            return;
          }
          else {
            if (this.modal == this.modals.config) {
              this.modals.config._handleCheckAll();
            }
            else if (this.modal == this.modals.rb) {
              this.modals.rb._handleCheckAll();
            }
            else if (this.modal == this.modals.custom && document.activeElement == this.modals.custom.input) {
              this.modals.custom.input.select();
            }
          }
        }
      }
      let ctrlSkip = ['c', 'x', 'v', 'z'].includes(key);
      if (ctrlSkip || !ev.ctrlKey && isInput) {
        return;
      }
    }

    // Board things
    if (ev.key == 'Alt' || ev.key == 'Meta') {
      if (ev.type == 'keydown') {
        this.toggleMenu(true);
      }
      else if (ev.type == 'keyup') {
        this.toggleMenu(false);
      }
    }
    if (ev.key == 'Shift') {
      this.config.shift = ev.type == 'keydown';
      this.config.setTool();
      return;
    }
    else if (ev.type == 'keyup') {
      return;
    }
    else if (ev.type == 'keydown' && !ev.repeat) {
      if (ev.ctrlKey) {
        if (key == 'z') {
          if (ev.shiftKey) {
            this.redo();
          }
          else {
            this.undo();
          }
        }
        else if (!ev.shiftKey && !ev.altKey) {
          if (key == 's') {
            this.save();
          }
          else if (key == 'o') {
            this.load();
          }
          else if (key == 'b') {
            this.toggleModal('rb');
          }
          else if (key == 'c') {
            this.clear();
          }
          else if (key == 'e') {
            this.toggleModal('resize');
          }
          else if (key == 'f') {
            this.toggleModal('custom');
          }
          else if (key == 'i') {
            this.import();
          }
          else if (key == 'g') {
            this.toggleModal('config');
          }
          else if (key == 'x') {
            this.handleClearStorage();
          }
          else if (key == 'a') {
            // preventDefault
          }
          else {
            return;
          }
        }
        else if (ev.shiftKey) {
          if (key == 's') {
            this.promptSaveImage();
          }
          else {
            return;
          }
        }
        else if (ev.altKey) {
          if (key == 's') {
            this.saveData();
          }
          else if (key == 'o') {
            this.loadData();
          }
          else {
            return;
          }
        }
        else {
          return;
        }
      }
      // ESC to hide/show controls
      else if (ev.key == 'Escape') {
        this.toggleToolHidden();
        this.toggleMenu(false);
      }

      // TAB to start/stop
      else if (ev.key == 'Tab' && !this.modal) {
        if (ev.shiftKey) {
          this.toggleRecord();
        }
        else {
          this.togglePlay();
        }
      }

      // SPACE to step or stop
      else if (ev.key == ' ') {
        if (this.running) {
          this.togglePlay();
        }
        else {
          this.step();
        }
      }
      // F1 to show documentation
      else if (ev.key == 'F1' || ev.key == '?') {
        this.showDoc();
      }
      // Tool and lesser keys
      else if (ev.key == 'g') {
        this.config.setTool('fill');
      }
      else if (ev.key == 'b') {
        this.config.setTool('brush');
      }
      else if (ev.key == 'f') {
        this.config.setTool('hexfilled');
      }
      else if (ev.key == 'h') {
        this.config.setTool('hexoutline');
      }
      else if (ev.key == 'l') {
        this.config.setTool('line');
      }
      else if (ev.key == '/') {
        this.config.setTool('lockline');
      }
      else if (ev.key == 'm') {
        this.config.setTool('move');
      }
      else if (ev.key == '1') {
        this.config.setToolSize(1);
      }
      else if (ev.key == '2') {
        this.config.setToolSize(2);
      }
      else if (ev.key == '3') {
        this.config.setToolSize(3);
      }
      else if (key == 'r') {
        this.resize();
      }
      else if (ev.key == 'c') {
        this.config.setPaintColorMode();
      }
      else if (ev.key == 'q') {
        this.saveSnapshot();
      }
      else if (ev.key == 'a') {
        this.loadSnapshot();
      }
      else if (ev.shiftKey && this.config.colorMode && ev.key == 'ArrowUp') {
        let newColor = Hexular.math.mod(this.config.paintColors[1] - 1, this.colorButtons.length);
        this.config.setPaintColor(1, newColor);
      }
      else if (ev.shiftKey && this.config.colorMode && ev.key == 'ArrowDown') {
        let newColor = Hexular.math.mod(this.config.paintColors[1] + 1, this.colorButtons.length);
        this.config.setPaintColor(1, newColor);
      }
      else if (this.config.colorMode && ev.key == 'ArrowUp') {
        let newColor = Hexular.math.mod(this.config.paintColors[0] - 1, this.colorButtons.length);
        this.config.setPaintColor(0, newColor);
      }
      else if (this.config.colorMode && ev.key == 'ArrowDown') {
        let newColor = Hexular.math.mod(this.config.paintColors[0] + 1, this.colorButtons.length);
        this.config.setPaintColor(0, newColor);
      }
      else {
        return;
      }
    }
    ev.preventDefault();
  }

  handleMouse(ev) {
    if (ev.type == 'mousedown') {
      // Close config menu if applicable;
      if (this.configMenu) {
        let target =  ev.target;
        while (target !=  this.buttons.toggleMenu && target.parentNode && (target = target.parentNode));
        if (target != this.buttons.toggleMenu) {
          this.toggleMenu(false);
        }
      }
      if (ev.target == this.fg && this.selected && !this.action) {
        if (ev.buttons & 1) {
          this.startAction(ev);
        }
        else if (ev.buttons & 2) {
          let setState = this.config.getPaintColor(1);
          this.startAction(ev, {setState});
        }
      }
      this.clickTarget = ev.target;
    }
    else if (ev.type == 'mouseup') {
      if (this.action)
        this.endAction(ev);
      else if (this.modalTranslate) {
        this.translateModal();
      }
      else if (this.clickTarget == ev.target) {
        if (ev.target == this.overlay) {
          this.toggleModal();
        }
        else if (ev.target == this.message) {
          this.clearMessage()
        }
        this.clickTarget = null;
      }
    }
    else if (ev.type == 'mousemove') {
      let cell;
      if (ev.target == this.fg) {
        this.selectCell([ev.pageX, ev.pageY]);
        this.moveAction(ev);
      }
      else if (this.modalTranslate) {
        this.translateModal([ev.pageX, ev.pageY]);
      }
      else {
        this.selectCell();
      }
      if (ev.target != this.info) {
        this.setCursorInfoInfo();
      }
    }
    else if (ev.type == 'mouseout') {
      this.selectCell();
    }
  }

  handleTouch(ev) {
    // Close config menu if applicable;
    if (this.configMenu && ev.target != this.buttons.toggleMenu) {
      setTimeout(() => this.toggleMenu(false), 500);
    }

    if (ev.target == this.fg) {
      if (ev.touches.length == 1) {
        let [x, y] = [ev.touches[0].pageX, ev.touches[0].pageY];
        if (ev.type == 'touchstart') {
          this.selectCell([x, y]);
          this.startAction(ev);
        }
        if (ev.type == 'touchmove') {
          this.selectCell([x, y]);
          this.moveAction(ev);
        }
        ev.preventDefault();
      }
      else if (ev.touches.length == 2) {
        if (ev.type == 'touchstart') {
          this.config.setTool('pinch', this.config.tool);
          this.startAction(ev);
          this.config.setTool();
        }
        if (ev.type == 'touchmove') {
          this.moveAction(ev);
        }
      }
      if (ev.type == 'touchend') {
        this.endAction(ev);
        this.selectCell();
      }
    }
  }

  startAction(ev, ...args) {
    let ctrl = ev.ctrlKey;
    let shift = ev.shiftKey;
    let Class = this.toolClasses[this.config.tool];
    this.action = new Class(this, {ctrl, shift}, ...args);
    this.action.start(ev);
    this.setToolInfo();
  }

  moveAction(ev) {
    if (this.action) {
      this.action.move(ev);
      this.setToolInfo();
    }
  }

  endAction(ev) {
    this.action && this.action.end(ev);
    this.action = null;
    this.setToolInfo();
  }

  // Cell selection and setting

  selectCell(coord) {
    this.selected = coord && this.cellAt(coord);
    this.drawSelectedCell();
  }

  drawSelectedCell() {
    let cell = this.selected;
    if (!this.action) {
      this.fgAdapter.clear();
      if (cell) {
        let color = this.config.selectColor;
        let width = this.config.selectWidth;
        width = (width + width / this.scale) / 2;
        let size = this.sizableTools.includes(this.config.tool) ? this.config.toolSize : 1;
        let opts = {stroke: true, lineWidth: width, strokeStyle: color};
        let radius = this.config.cellRadius;
        if (size == 1) {
          opts.type = Hexular.enums.TYPE_POINTY;
        }
        else {
          opts.type = Hexular.enums.TYPE_FLAT;
          radius = radius * (size * 2 - 1) * Hexular.math.apothem;
        }
        this.fgAdapter.drawHexagon(cell, radius, opts);
      }
    }
  }

  cellAt([x, y]) {
    [x, y] = this.windowToModel([x, y]);
    return this.model.cellAt([x, y]);
  }

  // TODO: Use Hexular.math.matrixMult
  windowToModel([x, y]) {
    x -= this.translateX;
    y -= this.translateY;
    x -= this.offsetX;
    x -= this.offsetY;
    x = x / this.scale;
    y = y / this.scale;
    return [x, y];
  }

  modelToWindow([x, y]) {
    x = x * this.scale;
    y = y * this.scale;
    x += this.offsetX;
    y += this.offsetY;
    x += this.translateX;
    y += this.translateY;
    return [x, y];
  }

  defaultTimingFn(i, n) {
    let t = i / n;
    let s = t * t;
    return s / (2 * (s - t) + 1);
  }

  // Alert messages

  setMessage(message, className) {
    let idx = ++this.msgIdx;
    className = className || 'alert';
    this.message.classList = 'message active ' + className;
    this.message.innerHTML = message;
    clearTimeout(this.messageTimer);
    this.messageTimer = setTimeout(() => {
      if (this.msgIdx == idx)
        this.clearMessage();
    }, 4000);
  }

  clearMessage() {
    this.message.className = 'message';
    requestAnimationFrame(() => this.message.innerHTML = '');
    clearTimeout(this.messageTimer);
  }
}

class Config {
  static get defaults() {
    return {
      preset: 'default',
      theme: 'light',
      radius: 60,
      cellRadius: 10,
      defaultScale: 1,
      numStates: 12,
      maxNumStates: 12,
      groundState: 0,
      defaultRule: 'identityRule',
      nh: 6,
      filters: {
        binaryFilter: false,
        deltaFilter: false,
        clipBottomFilter: false,
        clipTopFilter: false,
        modFilter: false,
        edgeFilter: false,
      },
      undoStackSize: 64,
      mobileRadius: 30,
      mobileDefaultScale: 1.5,
      mobileUndoStackSize: 16,
      interval: 125,
      autopause: true,
      pageBackground: '#f8f8f8',
      modelBackground: '#ffffff',
      showModelBackground: true,
      selectWidth: 2,
      selectColor: '#ffbb33',
      cellGap: 1,
      cellBorderWidth: 2,
      colors: Hexular.DEFAULTS.colors,
      availableRules: Config.merge({}, Rules),
      rules: Array(this.maxNumStates).fill(this.defaultRule),
      themes: Config.merge(Themes),
      presets: Config.merge({}, Presets),
      arrayType: 'Int8Array',
      defaultImageFilenameBase: 'hex',
      defaultArchiveFilename: 'hexular.ar',
      defaultFilename: 'hexular.bin',
      defaultVideoFilename: 'hexular.webm',
      defaultSettingsFilename: 'hexular.json',
      recordingMode: false,
      codec: 'vp9',
      videoBitsPerSecond: 120e6,
      scaleFactor: 1,

      tool: 'brush',
      shiftTool: 'move',
      toolSize: 1,
      colorMode: 0,
      paintColors: [1, 0],
      steps: 0,
      rbName: 'newElementaryRule',
      rbMiss: 0,
      rbMatch: 1,
      rbMissRel: 0,
      rbMatchRel: 0,
      rbRel: 0,
      rbStates: Array(64).fill(false),
      onDrawAvailable: [
        'sortCellsAsc',
        'sortCellsDesc',
      ],
      onDraw: null,
      onDrawCellAvailable: [
        'drawFilledPointyHex',
        'drawOutlinePointyHex',
        'drawFilledFlatHex',
        'drawOutlineFlatHex',
        'drawFilledCircle',
        'drawOutlineCircle',
      ],
      onDrawCell: 'drawFilledPointyHex',
      customInput: null,
      localStorageObj: window.localStorage,
      sessionStorageObj: window.sessionStorage,
    };
  }

  static merge(...objs) {
    let base = objs.shift();
    let next;
    let mergeWhitelist = [Object, Array];
    while (next = objs.shift()) {
      for (let [key, val] of Object.entries(next)) {
        if (val == null) continue;
        let defaultBaseVal = Array.isArray(val) ? [] : typeof val == 'object' ? {} : null;
        let baseVal = base[key] || defaultBaseVal;
        if (typeof val == 'object' && !mergeWhitelist.includes(val.constructor)) {
          base[key] = val;
        }
        else if (Array.isArray(val) && Array.isArray(baseVal)) {
          base[key] = Config.merge([], baseVal, val);
        }
        else if (typeof baseVal =='object' && typeof val == 'object') {
          base[key] = Config.merge({}, baseVal, val);
        }
        else {
          base[key] = val;
        }
      }
    }
    return base;
  }

  static toObject(kvArray) {
    let obj = {};
    for (let [key, value] of kvArray)
      obj[key] = value;
    return obj;
  }

  constructor(board, ...args) {
    this.board = board;
    Config.merge(this, Config.defaults);
    Object.entries(this).forEach(([key, value]) => {
      if (Array.isArray(value)) {
        this[key] = value.slice();
      }
      if (this.filters[key]) {
        this.filters[key] = value;
        delete this[key];
      }
    });

    // Restore state from local/session storage
    this.restoreState();

    // Finally, merge in URL parameter and constructor args
    Config.merge(this, new OptParser(this), ...args);

    // Set logical size and scale small boards
    let width = this.radius * this.cellRadius * Hexular.math.apothem * 4;
    let height = this.radius * this.cellRadius * 3;
    let scaleThreshold = 10 / 12;
    let scaleRatio = 1;
    if (width < window.innerWidth * scaleThreshold) {
      scaleRatio = window.innerWidth * scaleThreshold / width;
    }
    if (height < window.innerHeight * scaleThreshold) {
      scaleRatio = window.innerWidth * scaleThreshold / width;
    }
    this.scaleRatio = scaleRatio;
    this.cellRadius *= scaleRatio;
    width *= scaleRatio;
    height *= scaleRatio;
    this.logicalWidth = width;
    this.logicalHeight = height;

    this.mobile && document.body.classList.add('mobile');
  }

  initialize() {
    try {
      this.model = this.board.model;
      this.configModal = this.board.modals.config;
      this.configModal.update();
      this.rbModal = this.board.modals.rb;
      this.resizeModal = this.board.modals.resize;
      this.customModal = this.board.modals.custom;

      // Adapter
      this.setShowModelBackground();

      // Board
      this.setTheme(this.theme);
      if (!this.theme)
        this.setThemable();
      this.setPaintColor(0, this.paintColors[0]);
      this.setPaintColor(1, this.mobile ? -1 : this.paintColors[1]);
      this.setPaintColorMode(this.colorMode);
      this.setTool(this.tool);
      this.setToolSize(this.toolSize);
      this.setSteps(this.steps);
      this.setDefaultScale(this.defaultScale);

      // Config modal
      this.setPreset(this.preset);
      if (!this.preset) {
        this.setNh(this.nh);
        this.setNumStates(this.numStates);
        this.setRules();
        this.setFilters();
      }

      // Rule builder modal
      this.rbModal.ruleName.value = this.rbName || Config.defaults.rbName;
      this.setRbMiss([this.rbMiss, this.rbMissRel]);
      this.setRbMatch([this.rbMatch, this.rbMatchRel]);
      this.rbModal.stateElements.forEach((e, i) => {
        this.rbStates[i] && e.classList.add('active');
      });
      this.rbModal.updateRuleString();
      this.rbModal.update();

      // Appearance aka resize modal
      this.resizeModal.update();
      this.resizeModal.reset();

      // Custom code modal
      this.setCustomInput(this.customInput);
    }
    catch (error) {
      console.error(error);
      if (!this.error) {
        this.localStorageObj.clear();
        Board.resize({error});
      }
    }
  }

  // --- ADDERS, IMPORT/EXPORT ---

  addRule(ruleName, fn) {
    this.availableRules[ruleName] = fn;
    this.configModal.update();
    this.rbModal.update();
    this.setRules();
    this.storeLocalConfig();
  }

  addPreset(presetName, preset) {
    this.presets[presetName] = preset
    this.configModal.update();
    this.storeLocalConfig();
  }

  addTheme(themeName, themeObj) {
    this.themes[themeName] = this.getThemeFromObject(themeObj || this);
    this.resizeModal.update();
    this.storeLocalConfigAsync();
  }

  exportPreset() {
    return {
      preset: this.preset,
      numStates: this.numStates,
      rules: this.rules.slice(),
      nh: this.nh,
      filters: Object.assign({}, this.filters),
    };
  }

  resize(radius=this.radius) {
    this.radius = radius;
    this.storeSessionConfig();
    Board.resize(radius);
  }

  // --- SETTERS ---

  setAutopause(value) {
    this.autopause = value;
    if (value)
      this.resizeModal.autopause.classList.add('active');
    else
      this.resizeModal.autopause.classList.remove('active');
    this.storeSessionConfigAsync();
  }

  setBackground(type, color) {
    type = type ? [type] : ['pageBackground', 'modelBackground'];
    type.forEach((key) => {
      let thisColor = this[key] = color || this[key];
      if (key == 'pageBackground'){
        document.body.style.backgroundColor = thisColor;
      }
      else {
        this.board.bgAdapter.backgroundColor = thisColor;
        this.board.fgAdapter.backgroundColor = thisColor;
      }
      this.resizeModal[key].jscolor.fromString(thisColor || 'transparent');
    });

    this.checkTheme();
    this.storeSessionConfigAsync();
  }

  setCellGap(width) {
    this.cellGap = width != null ? width : this.cellGap;
    this.resizeModal.cellGap.value = this.cellGap;
    this.updateAdapter();
  }

  setCellBorderWidth(width) {
    this.cellBorderWidth = width != null ? width : this.cellBorderWidth;
    this.resizeModal.cellBorderWidth.value = this.cellBorderWidth;
    this.updateAdapter();
  }

  setCustomInput(value) {
    this.customInput = value || this.customInput;
    this.customModal.input.value = this.customInput || '';
    this.storeSessionConfigAsync();
  }

  updateAdapter() {
    this.board.bgAdapter.cellGap = this.cellGap * this.scaleRatio;
    this.board.fgAdapter.cellGap = this.cellGap * this.scaleRatio;
    this.board.bgAdapter.cellBorderWidth = this.cellBorderWidth * this.scaleRatio;
    this.board.fgAdapter.cellBorderWidth = this.cellBorderWidth * this.scaleRatio;
    this.board.bgAdapter.updateMathPresets();
    this.board.fgAdapter.updateMathPresets();
    this.checkTheme();
    this.board.draw();
    this.storeSessionConfigAsync();
  }

  setColor(idx, color) {
    this.colors[idx] = color;
    this.board.bgAdapter.fillColors[idx] = color;
    this.board.bgAdapter.strokeColors[idx] = color;
    this.board.fgAdapter.fillColors[idx] = color;
    this.board.fgAdapter.strokeColors[idx] = color;
    this.board.colorButtons[idx].style.backgroundColor = color;
    this.configModal.ruleMenus[idx].button.style.backgroundColor = color;
    this.resizeModal.colors[idx].jscolor.fromString(color);
    this.resizeModal.colors[idx].value = color;
    this.checkTheme();
    this.storeSessionConfigAsync();
  }

  setColors(colors=[]) {
    this.colors = Config.merge(this.colors, colors);
    this.board.bgAdapter.fillColors = this.colors.slice();
    this.board.bgAdapter.strokeColors = this.colors.slice();
    this.board.fgAdapter.fillColors = this.colors.slice();
    this.board.fgAdapter.strokeColors = this.colors.slice();
    for (let i = 0; i < 12; i++) {
      this.board.colorButtons[i].style.backgroundColor = this.colors[i];
      this.configModal.ruleMenus[i].button.style.backgroundColor = this.colors[i];
      this.resizeModal.colors[i].jscolor.fromString(this.colors[i]);
      this.resizeModal.colors[i].value = this.colors[i];
    }
    this.checkTheme();
    this.storeSessionConfigAsync();
  }

  setDefaultScale(scale) {
    this.defaultScale = scale;
    this.board.scaleTo(scale);
    if (this.resizeModal.scale.value != '0') {
      let sliderValue = Math.max(Math.min(scale, this.resizeModal.scaleMax), this.resizeModal.scaleMin);
      this.resizeModal.scale.value = sliderValue;
    }
    this.resizeModal.scaleIndicator.innerHTML = scale;
    this.storeSessionConfigAsync();
  }

  setFilter(filter, value) {
    let oldValue = this.filters[filter];
    this.filters[filter] = value;
    if (oldValue == value)
      return;
    this.model.clearFilters();
    Object.entries(this.filters).forEach(([filter, value]) => {
      if (value) {
        this.model.addFilter(Hexular.filters[filter]);
        this.configModal.filters[filter].classList.add('active');
      }
      else {
        this.configModal.filters[filter].classList.remove('active');
      }
    });
    this.checkPreset();
    this.storeSessionConfig();
  }

  setFilters() {
    this.model.clearFilters();
    Object.values(this.configModal.filters).forEach((e) => e.classList.remove('active'));
    Object.entries(this.filters).forEach(([filter, value]) => {
      if (value) {
        this.model.addFilter(Hexular.filters[filter]);
        this.configModal.filters[filter].classList.add('active');
      }
    });
    this.storeSessionConfig();
  }

  setNh(nh) {
    this.nh = nh;
    this.model.setNeighborhood(nh);
    this.configModal.selectNh.value = nh;
    this.checkPreset();
    this.storeSessionConfig();
  }

  setNumStates(num) {
    if (num)
      this.configModal.numStates.value = num;
    const numStates = parseInt(this.configModal.numStates.value);
    this.numStates = this.model.numStates = numStates;
    this.configModal.ruleMenus.forEach((ruleMenu) => {
      let disabled = ruleMenu.idx >= numStates;
      ruleMenu.container.setAttribute('data-disabled', disabled);
    });
    this.checkPreset();
    this.storeSessionConfig();
  }

  setOnDraw(fnName) {
    this.onDraw = fnName;
    Object.values(this.resizeModal.onDraw).forEach((e) => e.classList.remove('active'));
    let fns = this.onDrawAvailable.map((e) => this.board.bgAdapter[e]);
    this.board.bgAdapter.onDraw.replace(
      this.board.bgAdapter.onDraw.filter((e) => !fns.includes(e))
    );
    if (this.onDrawAvailable.includes(this.onDraw)) {
      this.resizeModal.onDraw[this.onDraw].classList.add('active');
      this.board.bgAdapter.onDraw.unshift(this.board.bgAdapter[this.onDraw]);
    }
    this.checkTheme();
    this.storeSessionConfigAsync();
  }

  setOnDrawCell(fnName) {
    this.onDrawCell = fnName || this.onDrawCell;
    Object.values(this.resizeModal.onDrawCell).forEach((e) => e.classList.remove('active'));
    this.resizeModal.onDrawCell[this.onDrawCell].classList.add('active');
    let fns = this.onDrawCellAvailable.map((e) => this.board.bgAdapter[e]);
    let curIdx = this.board.bgAdapter.onDrawCell.findIndex((e) => fns.includes(e));
    let del = 1;
    if (curIdx == -1) {
      curIdx = 0;
      del = 0;
    }
    this.board.bgAdapter.onDrawCell.splice(curIdx, del, this.board.bgAdapter[this.onDrawCell]);
    this.checkTheme();
    this.storeSessionConfigAsync();
  }

  setPaintColor(idx, color) {
    this.paintColors[idx] = color;
    let className = `active-${idx}`;
    this.board.colorButtons.forEach((e) => e.classList.remove(className));
    this.board.colorButtons[color] && this.board.colorButtons[color].classList.add(className);
    this.storeSessionConfigAsync();
  }

  getPaintColor(idx) {
    let offset = idx ? -1 : 1;
    if (this.colorMode)
      return this.paintColors[idx];
    else
      return Hexular.math.mod(this.board.selected.state + offset, this.numStates);
  }

  setPaintColorMode(mode) {
    this.colorMode = mode != null ? mode : +!this.colorMode;
    if (this.colorMode) {
      this.board.toolMisc.color.classList.add('active');
      this.board.menus.color.classList.remove('hidden');
    }
    else {
      this.board.menus.color.classList.add('hidden');
      this.board.toolMisc.color.classList.remove('active');
    }
    this.storeSessionConfigAsync();
  }

  setPreset(presetName) {
    const preset = this.presets[presetName];
    if (!preset) {
      this.configModal.selectPreset.selectedIndex = 0;
      this.configModal.addPreset.disabled = false;
      this.configModal.savePreset.disabled = true;
      this.preset = null;
      this.storeSessionConfig();
      return;
    }
    this.setNumStates(preset.numStates);
    this.setNh(preset.nh);

    this.rules = Object.assign(this.rules, preset.rules);

    this.filters = Object.assign({}, preset.filters);
    this.defaultRule = preset.defaultRule;
    this.setRules();
    this.setFilters();
    this.configModal.selectPreset.value = presetName;
    this.configModal.addPreset.disabled = true;
    this.configModal.savePreset.disabled = false;
    this.preset = presetName;
    this.storeSessionConfig();
  }

  setRule(idx, rule) {
    let fn = this.availableRules[rule];
    if (!fn) {
      fn = this.availableRules[this.defaultRule];
      rule = this.defaultRule;
    }
    if (idx != null) {
      this.configModal.ruleMenus[idx].select.value = rule;
      this.rules[idx] = rule;
      this.model.rules[idx] = fn;
    }
    else {
      this.configModal.defaultRuleMenu.select.value = rule;
      this.defaultRule = rule;
      this.model.defaultRule = fn;
    }
    this.checkPreset();
    this.storeSessionConfig();
  }

  setRules() {
    this.rules.forEach((rule, idx) => {
      let fn = this.availableRules[rule];
      if (!fn) {
        fn = this.availableRules[this.defaultRule];
        rule = this.defaultRule;
      }
      this.configModal.ruleMenus[idx].select.value = rule;
      this.model.rules[idx] = fn;
    });
    this.configModal.defaultRuleMenu.select.value = this.defaultRule;
    this.model.defaultRule = this.availableRules[this.defaultRule];
    this.checkPreset();
    this.storeSessionConfig();
  }

  setRbName(ruleName) {
    ruleName = ruleName || this.rbModal.ruleName.value || this.rbName;
    ruleName = ruleName.length != 0 ? ruleName : null;
    this.rbName = ruleName;
    if (ruleName)
      this.rbModal.ruleName.value = this.rbName;
    this.storeSessionConfigAsync();
  }

  setRbMiss(tuple) {
    let [miss, missRel] = tuple || this._strToTuple(this.rbModal.ruleMiss.value);
    this.rbMiss = miss;
    this.rbMissRel = missRel;
    this.rbModal.ruleMiss.value = this._tupleToStr([miss, missRel]);
    this.rbModal.updateRuleString();
    this.storeSessionConfigAsync();
  }

  setRbMatch(tuple) {
    let [match, matchRel] = tuple || this._strToTuple(this.rbModal.ruleMatch.value);
    this.rbMatch = match;
    this.rbMatchRel = matchRel;
    this.rbModal.ruleMatch.value = this._tupleToStr([match, matchRel]);
    this.rbModal.updateRuleString();
    this.storeSessionConfigAsync();
  }

  setShowModelBackground(value) {
    value = this.showModelBackground = value != null ? value : this.showModelBackground;
    let hookList = this.board.bgAdapter.onDraw;
    let fn = this.board.bgAdapter.drawCubicBackground;
    // We don't, at this point, care about order
    if (value)
      hookList.includes(fn) || hookList.unshift(fn);
    else
      hookList.includes(fn) && hookList.splice(hookList.indexOf(fn), 1);
    this.storeSessionConfigAsync();
  }

  setRecordingMode(value) {
    let hookList = this.board.bgAdapter.onDraw;
    let drawCubicBackground = this.board.bgAdapter.drawCubicBackground;
    let drawBackground = this.board.bgAdapter.drawBackground;
    if (value) {
      this.recordingMode = true;
      hookList.replace(hookList.filter((e) => e != drawCubicBackground));
      hookList.includes(drawBackground) || hookList.unshift(drawBackground);
    }
    else {
      this.recordingMode = false;
      hookList.replace(hookList.filter((e) => e != drawBackground));
      this.setShowModelBackground(this.showModelBackground);
    }
  }

  setSteps(steps) {
    steps = steps != null ? steps : this.steps;
    this.steps = steps;
    this.board.setInfoBox('steps', steps);
    this.storeSessionConfig();
  }

  setTheme(themeName) {
    if (this.themes[themeName]) {
      this.theme = themeName;
      this.resizeModal.selectTheme.value = themeName;
      this.resizeModal.addTheme.disabled = true;
      let theme = this.getThemeFromObject(this.themes[themeName]);
      Config.merge(this, theme);
      this.setThemable();
    }
    else {
      this.theme = null;
      this.resizeModal.selectTheme.value = null;
      this.resizeModal.addTheme.disabled = false;
    }
    this.board.draw();
    this.storeSessionConfigAsync();
  }

  setThemable() {
    this.setBackground();
    this.setColors();
    this.setCellGap();
    this.setCellBorderWidth();
  }

  setTool(tool, fallbackTool) {
    if (tool && this.board.toolClasses[tool]) {
      this.tool = tool;
      this.fallbackTool = fallbackTool || tool;
      this.storeSessionConfig();
    }
    else if (this.shift) {
      this.tool = this.shiftTool;
    }
    else if (this.tool != this.fallbackTool) {
      this.tool = this.fallbackTool;
    }
    Object.values(this.board.tools).forEach((e) => e.classList.remove('active'));
    if (this.board.tools[this.tool])
      this.board.tools[this.tool].classList.add('active');
    this.board.fg.setAttribute('data-tool', this.tool);
    this.board.drawSelectedCell();
  }

  setToolSize(size) {
    this.toolSize = size || 1;
    this.board.toolSizes.forEach((e) => e.classList.remove('active'));
    let selected = this.board.toolSizes[size - 1];
    selected && selected.classList.add('active');
    this.board.drawSelectedCell();
    this.storeSessionConfig();
  }

  // --- VALIDATION ---

  checkPreset() {
    const preset = this.presets[this.preset];
    if (!preset)
      return;
    let dirty = (() => {
      return this.model.numStates != preset.numStates ||
        this.nh != preset.nh ||
        this.defaultRule != preset.defaultRule ||
        this.rules.slice(0, this.model.numStates).reduce((a, rule, idx) => {
          return  a || preset.rules[idx] != rule;
        }, false) ||
        Object.entries(this.filters).reduce((a, [filter, value]) => {
          return a || preset.filters[filter] != value
        });
    })();
    if (dirty) {
      this.setPreset();
    }
  }

  checkTheme() {
    let theme = this.themes[this.theme];
    if (!theme || !this.theme) {
      this.setTheme();
      return;
    }
    theme = this.getThemeFromObject(this.themes[this.theme]);
    let dirty = false;
    if (theme.colors)
    for (let i = 0; i < this.maxNumStates; i ++) {
      if (theme.colors[i] != this.colors[i]) {
        dirty = true;
        break;
      }
    }
    dirty = dirty
      || theme.pageBackground != this.pageBackground
      || theme.modelBackground != this.modelBackground
      || theme.cellGap != this.cellGap
      || theme.cellBorderWidth != this.cellBorderWidth;
    if (dirty) {
      this.setTheme();
    }
  }

  getThemeFromObject(obj) {
    let args = [Config.defaults, obj].map((e) => {
      let {cellGap, cellBorderWidth, pageBackground, modelBackground, colors} = e;
      return {cellGap, cellBorderWidth, pageBackground, modelBackground, colors};
    });
    return Config.merge(...args);
  }

  // --- STORAGE ---

  getKeyValues(keys) {
    let obj = {};
    for (let key of keys)
      obj[key] = this[key];
    return Config.merge({}, obj);
  }

  getSessionConfig() {
    let sessionConfig = this.getKeyValues([
      'autopause',
      'cellBorderWidth',
      'cellGap',
      'codec',
      'colors',
      'colorMode',
      'customInput',
      'defaultRule',
      'defaultScale',
      'filters',
      'fallbackTool',
      'groundState',
      'interval',
      'maxNumStates',
      'modelBackground',
      'nh',
      'numStates',
      'onDraw',
      'onDrawCell',
      'pageBackground',
      'paintColors',
      'preset',
      'radius',
      'rbMiss',
      'rbMatch',
      'rbMissRel',
      'rbMatchRel',
      'rbName',
      'rbRel',
      'rbStates',
      'rules',
      'shiftTool',
      'showModelBackground',
      'steps',
      'theme',
      'tool',
      'toolSize'
    ]);
    return sessionConfig;
  };

  getLocalConfig() {
    let localConfig = this.getKeyValues([
      'availableRules',
      'presets',
      'themes',
    ]);
    Object.entries(localConfig.availableRules).forEach(([rule, fn]) => {
      localConfig.availableRules[rule] = fn.toString();
    });
    return localConfig;
  }

  retrieveConfig() {
    let sessionConfig = JSON.parse(this.sessionStorageObj.getItem('sessionConfig') || '{}');
    let localConfig = JSON.parse(this.localStorageObj.getItem('localConfig') || '{}');
    localConfig.availableRules = localConfig.availableRules || {};
    return {localConfig, sessionConfig}
  }

  restoreModel() {
    let modelState = this.loadModel('modelState');
    if (modelState) {
      this.board.newHistoryState();
      this.board.model.import(modelState);
      this.board.draw();
    }
  }

  restoreState(config) {
    if (this.model)
      this.restoreModel();
    config = config || this.retrieveConfig();
    let {localConfig, sessionConfig} = config;

    Object.entries(localConfig.availableRules).forEach(([rule, val]) => {
      let fn;
      try {
        val = eval(val);
        fn = Array.isArray(val) ? Hexular.util.ruleBuilder(...val) : val;
      }
      catch (e) {
        this.board.setMessage(`Error while loading rule "${rule}"`);
        console.error(e);
        console.trace();
      }
      if (typeof fn == 'function')
        localConfig.availableRules[rule] = fn;
      else
        delete localConfig.availableRules[rule];
    });

    let presets = localConfig.presets;
    if (presets) {
      localConfig.presets = {};
      Object.entries(presets).forEach(([presetName, preset]) => {
        localConfig.presets[presetName] = new Preset(preset);
      });
    }

    Config.merge(this, localConfig, sessionConfig);
    if (sessionConfig.preset !== undefined)
      this.preset = sessionConfig.preset;
  }

  storeSessionState(opts={}) {
    Object.entries(opts).forEach(([key, value]) => {
      this.sessionStorageObj.setItem(key, value);
    });
  }

  storeLocalState(opts={}) {
    Object.entries(opts).forEach(([key, value]) => {
      this.localStorageObj.setItem(key, value);
    });
  }

  storeSessionConfig() {
    let config = this.getSessionConfig();
    config.preset = config.preset || '';
    config.theme = config.theme || '';
    this.sessionStorageObj.setItem('sessionConfig', JSON.stringify(config));
  }

  storeSessionConfigAsync() {
    if (!this.pendingStoreSessionAsync) {
      this.pendingStoreSessionAsync = true;
      window.setTimeout(() => {
        this.storeSessionConfig();
        this.pendingStoreSessionAsync = null;
      }, 50);
    }
  }

  storeLocalConfig() {
    let config = this.getLocalConfig();
    this.localStorageObj.setItem('localConfig', JSON.stringify(config));
  }

  storeLocalConfigAsync() {
    if (!this.pendingStoreLocalAsync) {
      this.pendingStoreLocalAsync = true;
      window.setTimeout(() => {
        this.storeLocalConfig();
        this.pendingStoreLocalAsync = null;
      }, 50);
    }
  }

  storeModel(key, bytes, obj={}) {
    obj[key] = Array.from(bytes).map((e) => e.toString(36)).join('');
    this.storeSessionState(obj);
  }

  loadModel(key) {
    let str = this.sessionStorageObj.getItem(key);
    if (str) {
      let array = str.split('').map((e) => parseInt(e, 36));
      return new Int8Array(array);
    }
  }

  clearStorage() {
    let modelState = this.loadModel('modelState');
    this.sessionStorageObj.clear();
    this.localStorageObj.clear();
    this.storeModel('modelState', modelState);
  }

  _strToTuple(str) {
    return str.split(':').map((e) => parseInt(e)).map((e) => isNaN(e) ? null : e);
  }

  _tupleToStr(tuple) {
    return tuple.join(':');
  }
}
const EventHole = (...events) => {
  let handlerFn = () => {};
  let handler = (ev) => handlerFn(ev);
  events.map((e) => window.addEventListener(e, handler, {passive: false}));
  return (obj, fn) => {
    handlerFn = fn.bind(obj);
  };
};
class FileLoader {
  constructor(accept, ...args) {
    const defaults = {
      reader: 'readAsText',
      multiple: false,
      readIdx: 0,
      fileTypes: [],
      filterFn: () => Array(this.input.files.length).fill(true),
      loadFn: () => null,
      fileReader: new FileReader(),
      input: document.createElement('input'),
    };
    Object.assign(this, defaults, ...args);
    this.input.type = 'file';
    this.input.accept = accept;
    this.input.multiple = this.multiple;
    this.input.onchange = () => {
      this.readFilter = this.filterFn(Array.from(this.input.files));
      this._readNext();
    };
    this.fileReader.onloadend = (ev) => {
      ++this.readIdx < this.input.files.length && this._readNext();
    }
    this.fileReader.onload = (ev) => {
      this.loadFn(ev.target.result);
    };
  }

  prompt() {
    this.input.click();
  }

  set filter(fn) {
    this.filterFn = fn;
  }

  set onload(fn) {
    this.loadFn = fn;
  }

  _readNext() {
    let idx = this.readIdx;
    this.fileTypes[idx] = this.input.files[idx].type;
    this.readFilter[idx] && this.fileReader[this.reader](this.input.files[idx]);
  }
}
class Modal {
  constructor(board, name) {
    this.board = board;
    this.config = board.config;
    this.model = board.model;
    this.name = name;
    this.modal = document.querySelector(`.modal.${name}`);
    let title = this.modal.querySelector('.modal-title');
    if (title) {
      let closeBox = document.createElement('div');
      closeBox.className = 'close-box icon-clear';
      title.appendChild(closeBox);
      closeBox.onclick = (ev) => this.board.toggleModal();
      title.onmousedown = (ev) => this.board.translateModal([ev.pageX, ev.pageY]);
    }
  }

  open() {
    this.board.modal = this;
    this.modal.style.left = '0';
    this.modal.style.top = '0';
    this.reset();
    this.modal.classList.remove('hidden');
    this.board.overlay.classList.remove('hidden');
    let focus = this.modal.querySelector('.focus');
    focus && focus.focus();
  }

  close() {
    this.board.modal = null;
    this.board.overlay.classList.add('hidden');
    this.modal.classList.add('hidden');
  }

  reset() {}

  upate() {}
}
class OptParser {
  constructor(defaults) {

    // Let us infer if this is a mobile browser and make some tweaks
    if (window.devicePixelRatio > 1 && screen.width < 640) {
      defaults.scaleFactor = window.devicePixelRatio;
      defaults.mobile = true;
      defaults.radius = defaults.mobileRadius;
      defaults.defaultScale = defaults.mobileDefaultScale;
      defaults.undoStackSize = defaults.mobileUndoStackSize;
    }

    this.splitFilter(location.href.split('?')[1] || '', '&').map((e) => e.split('='))
    .forEach(([key, value]) => {
      if (!value)
        return
      let result, match, idx;

      // Check for indicial assignment
      match = key.match(/(.+?)\[(\d+)\]/);
      [key, idx] = match ? [match[1], match[2]] : [key, null];
      let current = idx != null ? defaults[key][idx] : defaults[key];

      // Check if array
      match = value.match(/^\[(.+?)\]$/);
      if (match)
        result = this.merge(current, this.splitFilter(match[1], ',').map((e) => this.parseArg(e)));
      else
        result = this.parseArg(value);

      if (idx != null)
        defaults[key][idx] = result;
      else
        defaults[key] = result;
    });

    Config.merge(this, defaults);
  }

  splitFilter(str, split) {
    return str && str.split(split).filter((e) => e.trim()).filter((e) => e.length > 0) || [];
  }

  parseArg(arg) {
    let numArg = parseFloat(arg);
    if (!Number.isNaN(numArg))
      return numArg;
    if (arg == 'null' || arg == '-')
      return null
    else if (arg == 'undefined')
      return undefined;
    else if (arg == 'true')
      return true;
    else if (arg == 'false')
      return false;
    else
      return arg;
  }

  merge(a, b) {
    let length = Math.max(a.length, b.length);
    let c = Array(length);
    for (let i = 0; i < length; i++) {
      c[i] = b[i] != null ? b[i] : a[i];
    }
    return c;
  }
}
class Preset {
  static fromString(str) {
    let obj = JSON.parse(str);
    return new Preset(obj);
  }

  constructor(...args) {
    let defaults = {
      defaultRule: 'identityRule',
      nh: 6,
      filters: {
        binaryFilter: false,
        deltaFilter: false,
        clipBottomFilter: false,
        clipTopFilter: false,
        modFilter: true,
        edgeFilter: false
      },
      rules: [],
    };
    Config.merge(this, defaults);
    for (let arg of args)
      if (!arg)
        continue;
      else if (arg.length)
        this.rules = arg.slice();
      else
        Config.merge(this, arg);
    this.numStates = this.numStates || this.rules.length;
  }

  toString() {
    return JSON.stringify(this);
  }
}
class Recorder {
  constructor(board) {
    this.board = board;
    this.config = board.config;
    this.transferCanvas = new TransferCanvas(this.board);
  }

  draw() {
    this.transferCanvas.draw();
  }

  start() {
    this.stream = this.transferCanvas.canvas.captureStream();
    let constraints = {
      frameRate: 30,
    };
    let opts = {
      videoBitsPerSecond: this.config.videoBitsPerSecond,
    };
    let customCodec = `video/webm;codecs=${this.config.codec || 'vp9'}`;
    if (MediaRecorder.isTypeSupported(customCodec))
      opts.mimeType = customCodec;
    else if (MediaRecorder.isTypeSupported('video/webm;codecs=vp9'))
      opts.mimeType = 'video/webm;codecs=vp9';
    else
      opts.mimeType = 'video/webm';
    this.stream.getTracks()[0].applyConstraints(constraints);
    this.recorder = new MediaRecorder(this.stream, opts);
    let blobs = [];
    this.recorder.ondataavailable = (ev) => {
      if (ev.data && ev.data.size > 0) {
        blobs.push(ev.data);
      }
    };
    this.recorder.onstop = (ev) => {
      let buffer = new Blob(blobs, {type: 'video/webm'});
      let dataUri = window.URL.createObjectURL(buffer);
      this.board.promptDownload(this.config.defaultVideoFilename, dataUri);
    };
    this.recorder.start();
  }

  stop() {
    this.recorder.stop();
    this.stream.getTracks()[0].stop();
  }
}
// A wrapper in anticipation of using non-native select boxes at some point

class Select {
  static init(arg, ...args) {
    if (!arg)
      return Select.init(document.querySelectorAll('select'));
    else if (arg instanceof HTMLSelectElement)
      return new Select(arg, ...arg);
    else if (typeof arg == 'string')
      return Select.init(document.querySelectorAll(arg), ...args);
    else if (arg.length)
      return Array.from(arg).map((e) => Select.init(e, ...args));
  }

  constructor(elArg, ...args) {
    this._onchange = () => null;
    this.el = elArg instanceof HTMLSelectElement ? elArg : this._create(elArg);
    this.el.select = this;
    this.el.addEventListener('change', this.onchange);
  }

  set onchange(fn) {
    this.el.onchange = fn;
  }

  set oninput(fn) {
    this.el.oninput = fn;
  }

  set value(value) {
    this.el.value = value;
    if (this.el.selectedIndex == -1)
      this.el.selectedIndex = 0;
  }

  get value() {
    return this.el.value;
  }

  set selectedIndex(idx) {
    this.el.selectedIndex = idx;
  }

  get selectedIndex() {
    return this.el.selectedIndex;
  }

  set disabled(value) {
    this.el.disabled = value;
  }

  get disabled() {
    return this.el.disabled;
  }

  replace(opts, selected, keep=0) {
    let data = opts;
    if (opts.length) {
      data = {};
      opts.forEach((e) => data[e] = e);
    }
    this.el.options.length = keep;
    Object.entries(data).forEach(([key, value]) => {
      let option = document.createElement('option');
      option.value = key;
      option.text = value;
      option.selected = key == selected;
      this.el.appendChild(option);
    });
  }

  _create(...args) {
    let strArgs = [];
    let objArgs = {};
    for (let arg of args)
      if (typeof arg == 'string')
        strArgs.push(arg);
      else if (typeof arg == 'object')
        Object.assign(objArgs, arg);
    let [tag, className] = Object.assign(['div', ''], strArgs);
    let el = document.createElement(tag);
    className && (el.className = className)
    Object.entries(objArgs).forEach(([key, value]) => {
      el.setAttribute(key, value);
    });
    return el;
  }
}
class TransferCanvas {
  constructor(board) {
    this.board = board;
    this.canvas = document.createElement('canvas');
    this.ctx = this.canvas.getContext('2d');
    this.canvas.width = window.innerWidth;
    this.canvas.height = window.innerHeight;
    this.draw();
  }

  draw() {
    this.ctx.drawImage(
      this.board.bg,
      0, 0, this.board.bg.width, this.board.bg.height,
      0, 0, this.canvas.width, this.canvas.height
    );
  }
}

class ConfigModal extends Modal {
  constructor(...args) {
    super(...args);
    this.ruleMenus = [];
    this.defaultRuleMenu = null;
    this.filters = {
      clipBottomFilter: document.querySelector('#filter-clip-bottom'),
      clipTopFilter: document.querySelector('#filter-clip-top'),
      binaryFilter: document.querySelector('#filter-binary'),
      deltaFilter: document.querySelector('#filter-delta'),
      modFilter: document.querySelector('#filter-mod'),
      edgeFilter: document.querySelector('#filter-edge'),
    };
    this.checkState = null;
    this.ruleGroup = document.querySelector('#rule-group');
    this.numStates = document.querySelector('#num-states');
    this.addPreset = document.querySelector('#add-preset');
    this.savePreset = document.querySelector('#save-preset');
    this.loadPreset = document.querySelector('#load-preset');
    this.selectPreset = document.querySelector('#select-preset').select;
    this.checkAll = document.querySelector('#check-all');
    this.setAll = document.querySelector('#set-all').select;
    this.selectNh = document.querySelector('#select-neighborhood').select

    this.modal.onmouseup = (ev) => this._handleCheckState(ev);
    this.modal.onmousemove = (ev) => this._handleCheckState(ev);
    this.modal.onmouseleave = (ev) => this._handleCheckState(ev);
    this.numStates.onchange = (ev) => this._handleNumStates();
    this.addPreset.onclick = (ev) => this._handleAddPreset();
    this.savePreset.onclick = (ev) => this._handleSavePreset();
    this.loadPreset.onclick = (ev) => this._handleLoadPreset();
    this.selectPreset.onchange = (ev) => this._handlePreset();
    this.checkAll.onclick = (ev) => this._handleCheckAll();
    this.setAll.oninput = (ev) => this._handleSetAll(ev);
    this.selectNh.onchange = (ev) => this.handleNh(ev);
    Object.entries(this.filters).forEach(([filter, button]) => {
      button.onclick = (ev) => this._handleFilter(filter);
    });
  }

  update() {
    this._updateMenus();
  }

  _handleNumStates() {
    this.config.setNumStates(this.numStates.value);
    this.board.setMessage(`Set model to ${this.config.numStates} states`);
  }

  _handlePreset() {
    this.config.setPreset(this.selectPreset.value);
    this.board.setMessage(`Selected preset "${this.config.preset}"`);
  }

  _handleAddPreset() {
    // TODO: Replace native prompt
    let presetName = window.prompt('Please enter a preset name:');
    if (presetName) {
      let preset = new Preset(this.config.exportPreset());
      this.config.addPreset(presetName, preset);
      this.config.setPreset(presetName);
    }
  }

  _handleSavePreset() {
    let obj = {};
    let presetName = this.config.preset;
    obj[presetName] = this.config.exportPreset();
    let dataUri = `data:application/json,${encodeURIComponent(JSON.stringify(obj))}`;
    this.board.promptDownload(`${presetName.replace(/ /g, '_')}.json`, dataUri);
  }

  _handleLoadPreset() {
    let fileLoader = new FileLoader('.json');
    fileLoader.onload =  (result) => {
      try {
        let obj = JSON.parse(result);
        let presets = Object.entries(obj).map(([presetName, presetObj]) => {
          this.config.addPreset(presetName, new Preset(presetObj));
          return presetName;
        });

        if (presets.length > 1) {
          this.board.setMessage('Presets imported!');
        }
        else if (presets.length == 1) {
          this.board.setMessage('Preset imported!');
          this.config.setPreset(presets[0]);
        }
      }
      catch (e) {
        this.board.setMessage(e.toString(), 'error');
      }

    };
    fileLoader.filter = (files) => {
      let result = files.map((file) => file.type.indexOf('json') >= 0);
      result.some((e) => !e) && this.setMessage('Not all selected files are JSON files', 'error');
      return result;
    };
    fileLoader.prompt();
  }

  _handleCheckState(ev) {
    if (ev.buttons ^ 1)
      this.checkState = null;
  }

  _handleCheckAll() {
    let ruleMenus = this.ruleMenus.concat(this.defaultRuleMenu);
    let check = !ruleMenus.every((ruleMenu) => ruleMenu.checked);
    if (check)
      this.checkAll.classList.add('checked');
    else
      this.checkAll.classList.remove('checked');
    ruleMenus.forEach((ruleMenu) => {
      ruleMenu.checked = check;
    });
  }

  _handleSetAll(ev) {
    let rule = this.setAll.value;
    if (this.config.availableRules[rule]) {
      this.ruleMenus.concat(this.defaultRuleMenu)
      .filter((e) =>  e.checked)
      .forEach((ruleMenu) => {
        this.config.setRule(ruleMenu.idx, rule);
      });
    }
    this.setAll.value = null;
  }

  handleNh(ev) {
    this.config.setNh(parseInt(this.selectNh.value));
    this.board.setMessage(`Set neighborhood to N${this.config.nh}`)
  }

  _handleFilter(filter) {
    let state = !this.config.filters[filter];
    this.config.setFilter(filter, state);
  }

  _updateMenus() {
    this.availableRuleNames = Object.keys(this.config.availableRules);
    this.presetNames = Object.keys(this.config.presets);

    this.selectPreset.replace(this.presetNames, this.config.preset, 1);
    this.setAll.replace(this.availableRuleNames, null, 1);

    this.defaultRuleMenu = new RuleMenu(this, document.querySelector('#default-rule-menu'));
    while (this.ruleGroup.firstChild)
      this.ruleGroup.firstChild.remove();
    this.ruleMenus = [];
    for (let i = 0; i < this.config.maxNumStates; i++) {
      let ruleMenu = new RuleMenu(this, i);
      this.ruleMenus.push(ruleMenu);
      this.ruleGroup.appendChild(ruleMenu.container);
    }
  }

}

class RuleMenu {
  constructor(modal, arg) {
    this.modal = modal;
    this.board = modal.board;
    this.config = modal.config;
    if (typeof arg == 'number') {
      let prototype = document.querySelector('.assets .rule-menu');
      this.container = this.container = prototype.cloneNode(true);
      this.idx = arg;
    }
    else {
      this.container = arg;
      this.idx = null;
    }
    let container = this.container;
    let idx = this.idx;
    let select = this.select = new Select(container.querySelector('select'));
    let button = this.button = container.querySelector('button.checkable');
    select.ruleMenu = this;
    select.onchange = (ev) => this.config.setRule(idx, select.value);
    if (idx != null) {
      container.title = `State ${idx}`;
      button.style.backgroundColor = this.board.bgAdapter.colors[idx];
      select.replace(this.modal.availableRuleNames, this.config.rules[idx]);
    }
    else {
      container.title = 'Default rule';
      button.classList.add('icon-infinity');
      select.replace(this.modal.availableRuleNames, this.config.defaultRule);
    }
    container.setAttribute('data-disabled',  idx >= this.config.numStates);

    button.onmousedown = (ev) => {
      this.checked = !this.checked;
      this.modal.checkState = this.checked;
      ev.preventDefault();
    };
    button.onmousemove = (ev) => {
      if (this.modal.checkState != null)
        this.checked = this.modal.checkState;
    };
  }

  set checked(val) {
    if (val)
      this.container.classList.add('checked');
    else
      this.container.classList.remove('checked');
  }

  get checked() {
    return this.container.classList.contains('checked');
  }
}
class ConfirmModal extends Modal {
  constructor(...args) {
    super(...args);
    this.resolve = null;
    this.text = document.querySelector('#confirmation-text');
    this.buttonYes = document.querySelector('#confirmation-yes');
    this.buttonNo = document.querySelector('#confirmation-no');
    this.buttonNo.onclick = () => {
      this.close();
    }
  }

  close() {
    super.close();
    this.resolve && this.resolve();
  }
  async ask(msg) {
    this.text.innerHTML = msg;
    this.board.toggleModal(this.name);
    return await this._awaitButton();
  }

  _awaitButton() {
    return new Promise((resolve, reject) => {
      this.buttonYes.onclick = () => {
        this.resolve = null;
        resolve(true);
        this.close();
      };
      this.resolve = (value) => {
        this.resolve = null;
        resolve(false);
      };
    });
  }
}
class CustomModal extends Modal {
  constructor(...args) {
    super(...args);
    this.selectExample = document.querySelector('#select-example').select;
    this.input = document.querySelector('#custom-input');
    this.output = document.querySelector('#custom-output');
    this.import = document.querySelector('#import'),
    this.button = document.querySelector('#add-custom-code');

    this.selectExample.onchange = (ev) => {
      let str = Examples.customCodeDemos[this.selectExample.value];
      if (str) {
        if (document.execCommand) {
          this.input.focus();
          this.input.select();
          document.execCommand("delete", false, str);
          document.execCommand("insertText", false, str);
        }
        else {
          this.input.value = str;
        }
      }
    };

    this.input.oninput = (ev) => this.selectExample.value = null;

    this.input.onchange = (ev) => this.config.setCustomInput(this.input.value);

    this.output.onclick = (ev) => this.output.select();

    this.import.onclick = (ev) => this.board.import();

    this.button.onclick = (ev) => {
      if (this.input.value == '') {
        this.board.setMessage('Nothing to run!', 'error');
        return;
      }
      try {
        let evalFn = new Function('Hexular', 'Board', 'value', 'return eval(value)')
        let output = evalFn(Hexular, Board, this.input.value);
        this.output.value = output;
        this.board.setMessage('Done!');
      }
      catch (err) {
        this.board.setMessage(`An error occurred: ${err}.`, 'error');
      }
    }
  }

  reset() {
    this.selectExample.replace(Object.keys(Examples.customCodeDemos), null, 1);
    if (this.input.value == '')
      this.input.value = this.input.placeholder;
  }
}
class RbModal extends Modal {
  constructor(...args) {
    super(...args);
    this.ruleName = document.querySelector('#rule-name');
    this.selectAvailable = document.querySelector('#select-available').select;
    this.checkAll = document.querySelector('#rule-select-all');
    this.ruleMiss = document.querySelector('#rule-miss').select;
    this.ruleMatch = document.querySelector('#rule-match').select;
    this.stateGrid = document.querySelector('#state-grid');
    this.ruleString = document.querySelector('#rule-string');
    this.resetButton = document.querySelector('#reset-rule');
    this.addButton = document.querySelector('#add-rule');
    this.stateElements = [];
    this.settingState = null;
    this.updateRuleStringPending = false;

    while (this.stateGrid.firstChild)
      this.stateGrid.firstChild.remove();
    let template = document.querySelector('.statemask');
    this.config.rbStates.forEach((state, i) => {
      let item = template.cloneNode(true);
      this.stateElements.push(item);
      item.setAttribute('title', i);
      let nbrs = item.querySelectorAll('polygon');

      Array.from(nbrs).slice(1).forEach((nbr, j) => {
        let bit = (i >>> (5 - j)) % 2;
        if (!bit)
          nbr.classList.add('off');
      });
      this.stateGrid.appendChild(item);
      item.onmousedown = () => {
        this.settingState = !this.config.rbStates[i];
        this.setState(i);
      };
      item.onkeydown = (ev) => {
        if (ev.key == ' ' || ev.key == 'Enter') {
          this.setState(i, !this.config.rbStates[i]);
          this.updateRuleString();
          this.config.storeSessionConfigAsync();
        }
      }
      item.onmousemove = () => {
        this.settingState != null && this.setState(i);
      }
    });

    this.modal.onmouseup = this.modal.onmouseleave = () => {
      this.settingState = null;
      this.updateRuleString();
      this.config.storeSessionConfigAsync();
    };

    this.selectAvailable.onchange = () => {
      let rule = this.selectAvailable.value;
      this.config.setRbName(rule);
      let fn = this.config.availableRules[rule];
      if (fn) {
        let obj = JSON.parse(fn);
        delete obj[1].range;
        fn = JSON.stringify(obj);
        this.ruleString.value = fn.toString();
      }
      this.parseRuleString();
    };

    this.checkAll.onclick = () => this._handleCheckAll();

    this.ruleName.onchange = () => this.config.setRbName();

    this.ruleName.oninput = () => {
      if (this.ruleName.value.length > 0)
        this.addButton.disabled = false;
      else
        this.addButton.disabled = true;
    };

    this.ruleMiss.onchange = () => this.config.setRbMiss();
    this.ruleMatch.onchange = () => this.config.setRbMatch();

    this.ruleString.oninput = () => this.parseRuleString();
    this.ruleString.onfocus = () => this.ruleString.select();

    this.resetButton.onclick = () => this.clear();

    this.addButton.onclick = () => {
      let [rule, opts] = this.getRuleString();
      if (!rule) {
        rule = this._getMasks();
        opts = this._getOpts();
      }
      let fn = Hexular.util.ruleBuilder(rule, opts);
      this.config.addRule(this.ruleName.value, fn);
      this.board.setMessage(`Rule #${fn.n} added!`);
      console.log('Rule added:', [rule, opts]);
    };
  }

  clear() {
    this.config.setRbMiss([Config.defaults.rbMiss, Config.defaults.rbMissRel]);
    this.config.setRbMatch([Config.defaults.rbMatch, Config.defaults.rbMatchRel]);
    this.config.setRbName(Config.defaults.rbName);
    this.config.rbRel = Config.defaults.rbRel;
    this.setStates([], false);
    this.updateRuleString();
  }

  update() {
    let rbRules = Object.entries(this.config.availableRules).filter(([rule, fn]) => fn.n).map(([rule, fn]) => rule);
    this.selectAvailable.replace(rbRules, this.ruleName.value, 1);
  }

  setState(idx, value=this.settingState) {
    let states = this.config.rbStates;
    states[idx] = value
    let item = this.stateElements[idx];
    if (value) {
      item.classList.add('active');
      if (!states.some((e) => !e)) {
        this.checkAll.classList.add('active');
      }
    }
    else {
      item.classList.remove('active');
      this.checkAll.classList.remove('active');
    }
  }

  setStates(array, value=this.settingState) {
    let states = this.config.rbStates;
    states.fill(false);
    this.stateElements.forEach((e) => e.classList.remove('active'));
    array.forEach((idx) => {
      if (states[idx] == null)
        return;
      states[idx] = value;
      if (value)
        this.stateElements[idx].classList.add('active');
      else
        this.stateElements[idx].classList.remove('active');
    });
    if (states.filter((e) => e).length == 64)
      this.checkAll.classList.add('active');
    else
      this.checkAll.classList.remove('active');
  }

  getRuleString() {
    let rule, opts;
    try {
      [rule, opts] = JSON.parse(this.ruleString.value);
    }
    catch {};
    if (!Array.isArray(rule))
      rule = null;
    opts = opts || {};
    return [rule, opts];
  }

  updateRuleString() {
    if (!this.updateRuleStringPending) {
      this.updateRuleStringPending = true;
      requestAnimationFrame(() => {
        let [strRule, strOpts] = this.getRuleString();
        let configRule = this._getMasks();
        let [miss, missRel] = [this.config.rbMiss, this.config.rbMissRel];
        let [match, matchRel] = [this.config.rbMatch, this.config.rbMatchRel];
        let rel = this.config.rbRel;
        let rule = configRule ? configRule : strRule;
        let opts = Config.merge({}, strOpts, {miss, match, missRel, matchRel, rel});
        let ruleString  = JSON.stringify([rule, opts]);
        if (this.ruleString.value != ruleString) {
          this.ruleString.value = ruleString;
          this.selectAvailable.value = null;
        }
        this.updateRuleStringPending = false;
      });
    }

  }

  parseRuleString() {
    let [rules, opts] = this.getRuleString();
    if (rules) {
      this.setStates(rules, true);
      let {miss, match, missRel, matchRel, rel} = opts;
      this.config.rbRel = rel;
      this.config.setRbMiss([miss, missRel]);
      this.config.setRbMatch([match, matchRel]);
      this.config.storeSessionConfigAsync();
    }
  }

  _handleCheckAll() {
    this.settingState = this.checkAll.classList.toggle('active');
    for (let i = 0; i < 64; i++)
      this.setState(i);
    this.settingState = null;
    this.updateRuleString();
    this.config.storeSessionConfigAsync();
  }

  _getMasks() {
    return this.config.rbStates.map((e, i) => e && i).filter((e) => e !== false);
  }

  _getOpts() {
    return {
      miss: this.config.rbMiss,
      missRel: this.config.rbMissRel,
      match: this.config.match,
      matchRel: this.config.matchRel,
    };
  }
}
class ResizeModal extends Modal {
  constructor(...args) {
    super(...args);
    this.defaultInterval = Config.defaults.interval;
    this.radius = this.defaultRadius = Config.defaults.radius;
    this.colors = Array.from(document.querySelectorAll('.group.color input')).slice(2);
    this.pageBackground = document.querySelector('#page-bg');
    this.modelBackground = document.querySelector('#model-bg');
    this.onDraw = {
      sortCellsAsc: document.querySelector('#sort-cells-asc'),
      sortCellsDesc: document.querySelector('#sort-cells-desc'),
    };
    this.onDrawCell = {
      drawFilledPointyHex: document.querySelector('#draw-filled-pointy-hex'),
      drawOutlinePointyHex: document.querySelector('#draw-outline-pointy-hex'),
      drawFilledFlatHex: document.querySelector('#draw-filled-flat-hex'),
      drawOutlineFlatHex: document.querySelector('#draw-outline-flat-hex'),
      drawFilledCircle: document.querySelector('#draw-filled-circle'),
      drawOutlineCircle: document.querySelector('#draw-outline-circle'),
    };
    this.autopause = document.querySelector('#autopause');
    this.cellGap = document.querySelector('#cell-gap');
    this.cellBorderWidth = document.querySelector('#cell-border-width');
    this.selectTheme = document.querySelector('#select-theme').select;
    this.addTheme = document.querySelector('#add-theme');
    this.interval = document.querySelector('#interval-slider');
    this.intervalIndicator = document.querySelector('#interval-indicator');
    this.scale = document.querySelector('#scale-slider');
    this.scaleIndicator = document.querySelector('#scale-indicator');
    this.scaleMin = parseFloat(this.scale.min);
    this.scaleMax = parseFloat(this.scale.max);
    this.resize = document.querySelector('#resize-slider');
    this.resizeIndicator = document.querySelector('#resize-indicator');

    this.colors.forEach((el, idx) => {
      let pickerClosed = false;
      el.setAttribute('title', `Color ${idx}`);
      el.onchange = () => this.config.setColor(idx, el.value);
      el.onfocus = el.onclick = () => pickerClosed = false;
      el.onkeydown = (ev) => {
        if (ev.key == 'Escape' && !pickerClosed) {
          el.jscolor.hide();
          pickerClosed = true;
          ev.stopPropagation();
        }
      }
    });
    ['pageBackground', 'modelBackground'].forEach((key) => {
      let pickerClosed = false;
      let el = this[key];
      el.onchange = () => this.config.setBackground(key, el.value);
      el.onfocus = el.onclick = () => pickerClosed = false;
      el.onkeydown = (ev) => {
        if (ev.key == 'Escape' && !pickerClosed) {
          el.jscolor.hide();
          pickerClosed = true;
          ev.stopPropagation();
        }
      }
    });
    Object.entries(this.onDraw).forEach(([fnName, button]) => button.onclick = () => this._setOnDraw(fnName));
    Object.entries(this.onDrawCell).forEach(([fnName, button]) => button.onclick = () => this._setOnDrawCell(fnName));
    this.autopause.onclick = (ev) => this._setAutopause(!this.config.autopause);
    this.cellGap.onchange = (ev) => this._setCellGap(this.cellGap.value);
    this.cellBorderWidth.onchange = (ev) => this._setCellBorderWidth(this.cellBorderWidth.value);
    this.set = document.querySelector('#resize-set');
    this.selectTheme.onchange = (ev) => this._handleSelectTheme();
    this.addTheme.onclick = (ev) => this._handleAddTheme();
    this.interval.oninput = (ev) => this._updateInterval(this.interval.value);
    this.scale.oninput = (ev) => this._updateScale(this.scale.value);
    this.resize.oninput = (ev) => this._updateResize(this.resize.value);
    this.set.onclick = (ev) => this._resize();
  }

  reset() {
    this.selectTheme.value = this.config.theme;
    this._setAutopause();
    this._setCellGap(this.config.cellGap);
    this._setCellBorderWidth(this.config.cellBorderWidth);
    this._setOnDraw();
    this._setOnDrawCell();
    this.resize.value = this.config.radius;
    this.interval.value = this.config.interval;
    this._updateInterval();
    this._updateScale(this.config.defaultScale);
    this._updateResize();
  }

  update() {
    this.selectTheme.replace(Object.keys(this.config.themes).sort(), this.config.theme, 1);
  }

  _setAutopause(value) {
    this.config.setAutopause(value != null ? value : this.config.autopause);
  }

  _setCellGap(value) {
    this.config.setCellGap(parseFloat(value || 0));
    this.board.draw();
  }

  _setCellBorderWidth(value) {
    this.config.setCellBorderWidth(parseFloat(value || 0));
    this.board.draw();
  }

  _setOnDraw(fnName) {
    if (this.config.onDraw == fnName)
      this.config.setOnDraw();
    else
      this.config.setOnDraw(fnName || this.config.onDraw);
    this.board.draw();
  }

  _setOnDrawCell(fnName) {
    this.config.setOnDrawCell(fnName);
    this.board.draw();
  }

  _handleSelectTheme() {
    this.config.setTheme(this.selectTheme.value);
  }

  _handleAddTheme() {
    // TODO: Replace native prompt
    let themeName = window.prompt('Please enter a theme name:');
    if (themeName) {
      this.config.addTheme(themeName, this.config);
      this.config.setTheme(themeName);
    }
  }

  _updateInterval(value) {
    if (value != null)
      this.config.interval = parseInt(value) || this.defaultInterval;
    this.intervalIndicator.innerHTML = this.config.interval;
  }

  _updateScale(value) {
    value = parseFloat(value);
    if (value != this.config.defaultScale)
      this.config.setDefaultScale(value || 1);
  }

  _updateResize(value) {
    if (value != null)
      this.radius = parseInt(value) || this.defaultRadius;
    else
      this.radius = this.config.radius;

    this.resizeIndicator.innerHTML = this.radius;
    this.set.innerHTML =
      `<i class="icon-arrow-top-right-bottom-left"></i> Resize (${this.radius * (this.radius - 1) * 3 + 1} cells)`;
  }

  _resize() {
    this.config.resize(this.radius);
    Board.instance.setMessage(`Set board size to ${Board.config.radius}`);
  }
}
const Examples = (() => {
  let fnCount = 0;

  let customCodeDemos = {
    addRule: `Board.config.addRule('newRule', (cell) => cell.max > 2 ? cell.state + 1 : 0)`,
    maxFilledTriangle: `Examples.maxFilledTriangle()`,
    minOutlineCircle: `Examples.minOutlineCircle(3)`,
    drawCellImage: `
Examples.drawCellImage(null, {scale: 2, type: Hexular.enums.TYPE_FLAT, states: [1, 2], clip: true})
`,
    drawBackgroundImage: `Examples.drawBackgroundImage(null, {scale: 1})`,
    remove: `Examples.remove(/* index of example callback to remove */)`,
    scaleTo: `Board.instance.scaleTo(Board.instance.scale * 2, 5000)`,
  };

  Object.entries(customCodeDemos).forEach(([k, v]) => {
    customCodeDemos[k] = v.trim();
  });

  // Yes this is very inconsistent with the other two
  function hexagonFactory(defaultOpts, cb) {
    let defaults = {
      type: Hexular.enums.TYPE_POINTY,
      stroke: false,
      fill: false,
      fillStyle: null,
      strokeStyle: null,
    };
    if (!cb)
      cb = (a, b, c) => Math.max(a.state, b.state, c.state);
    defaultOpts = Object.assign({}, defaults, defaultOpts);
    return (radius, optOverrides) => {
      let opts = Object.assign({}, defaultOpts, optOverrides);
      let fn = function(cell) {
        let adapter = Board.bgAdapter;
        let model = Board.model;
        let fillStyle = opts.fillstyle;
        let strokeStyle = opts.strokeStyle;
        if (!cell.state)
          return;
        let slice = cell.with[6].nbrSlice;
        for (let i = 0; i < 5; i++) {
          let n1 = slice[i];
          let n2 = slice[(i + 1) % 6];
          if (n1.state && n2.state && !n1.edge && !n2.edge) {
            let state = cb(cell, n1, n2);
            let [x0, y0] = model.cellMap.get(cell);
            let [x1, y1] = model.cellMap.get(n1);
            let [x2, y2] = model.cellMap.get(n2);
            let x = (x0 + x1 + x2) / 3;
            let y = (y0 + y1 + y2) / 3;
            let r = radius || adapter.innerRadius;
            if (opts.stroke)
              opts.strokeStyle = strokeStyle || adapter.strokeColors[state];
            if (opts.fill)
              opts.fillStyle = fillStyle || adapter.fillColors[state];
            adapter.drawHexagon([x, y], r, opts);
          }
        }
      }
      fn.idx = ++fnCount;
      Board.bgAdapter.onDrawCell.push(fn);
      Board.instance.draw();
      return fn.idx;
    };
  }

  function circleFactory(cb) {
    return (radius) => {
      let fn = function(cell) {
        let adapter = Board.bgAdapter;
        let model = Board.model;
        if (!cell.state)
          return;
        let slice = cell.with[6].nbrSlice;
        for (let i = 0; i < 5; i++) {
          let n1 = slice[i];
          let n2 = slice[(i + 1) % 6];
          if (n1.state && n2.state && !n1.edge && !n2.edge) {
            let [x0, y0] = model.cellMap.get(cell);
            let [x1, y1] = model.cellMap.get(n1);
            let [x2, y2] = model.cellMap.get(n2);
            let x = (x0 + x1 + x2) / 3;
            let y = (y0 + y1 + y2) / 3;
            let r = radius || adapter.innerRadius;
            adapter.context.beginPath();
            adapter.context.arc(x, y, r, 0, Math.PI * 2);
            cb(adapter.context, cell, n1, n2);
          }
        }
      }
      fn.idx = ++fnCount;
      Board.bgAdapter.onDrawCell.push(fn);
      Board.instance.draw();
      return fn.idx;
    };
  }

  function triangleFactory(cb) {
    return (radius, opts) => {
      opts = Object.assign({
        center: true,
        vertex: false,
      }, opts);
      let vertices = Hexular.math.vertices.map(([x, y]) => [y, x]);
      let fn = function(cell) {
        let adapter = Board.bgAdapter;
        let model = Board.model;
        if (!cell.state)
          return;
        let slice = cell.with[6].nbrSlice;
        for (let i = 0; i < 5; i++) {
          let n1 = slice[i];
          let n2 = slice[(i + 1) % 6];
          if (n1.state && n2.state && !n1.edge && !n2.edge) {
            let [x0, y0] = model.cellMap.get(cell);
            let [x1, y1] = model.cellMap.get(n1);
            let [x2, y2] = model.cellMap.get(n2);
            let x = (x0 + x1 + x2) / 3;
            let y = (y0 + y1 + y2) / 3;
            let r = radius || adapter.innerRadius;
            let o = i % 2;
            for (let e of [opts.center, opts.vertex]) {
              o = o ^1;
              if (!e) continue;
              adapter.context.beginPath();
              adapter.context.moveTo(x + vertices[o][0] * r, y + vertices[o][1] * r);
              adapter.context.lineTo(x + vertices[o + 2][0] * r, y + vertices[o + 2][1] * r);
              adapter.context.lineTo(x + vertices[o + 4][0] * r, y + vertices[o + 4][1] * r);
              adapter.context.closePath();
              cb(adapter.context, cell, n1, n2);
            }
          }
        }
      };
      fn.idx = ++fnCount;
      Board.bgAdapter.onDrawCell.push(fn);
      Board.instance.draw();
      return fn.idx;
    };
  }

  function lineFactory(cb) {
    return (radius) => {
      let fn = function(cell) {
        let adapter = Board.bgAdapter;
        let model = Board.model;
        if (cell.edge)
          return;
        let slice = cell.with[6].nbrSlice;
        for (let i = 0; i < 5; i += 2) {
          let nbr = slice[i];
          if (!nbr.state || !cell.state || nbr.edge)
            continue;
          let [x0, y0] = model.cellMap.get(cell);
          let [x1, y1] = model.cellMap.get(nbr);
          adapter.context.beginPath();
          adapter.context.moveTo(x0, y0);
          adapter.context.lineTo(x1, y1);
          cb(adapter.context, cell, nbr);
        }
      };
      fn.idx = ++fnCount;
      Board.bgAdapter.onDrawCell.unshift(fn);
      Board.instance.draw();
      return fn.idx;
    };
  }

  function fillMax(ctx, ...cells) {
    let state = Math.max(...cells.map((e) => e.state));
    ctx.fillStyle = Board.bgAdapter.fillColors[state];
    ctx.fill();
  }

  function fillMin(ctx, ...cells) {
    let state = Math.min(...cells.map((e) => e.state));
    ctx.fillStyle = Board.bgAdapter.fillColors[state];
    ctx.fill();
  }

  function outlineMax(ctx, ...cells) {
    let lineWidth = Board.config.cellBorderWidth;
    if (lineWidth == 0)
      return;
    let state = Math.max(...cells.map((e) => e.state));
    ctx.strokeStyle = Board.bgAdapter.strokeColors[state];
    ctx.lineWidth = lineWidth;
    ctx.lineJoin = "round";
    ctx.lineCap = "round";
    ctx.stroke();
  }

  function outlineMin(ctx, ...cells) {
    let lineWidth = Board.config.cellBorderWidth;
    if (lineWidth == 0)
      return;
    let state = Math.min(...cells.map((e) => e.state));
    ctx.strokeStyle = Board.bgAdapter.strokeColors[state];
    ctx.lineWidth = lineWidth;
    ctx.lineJoin = "round";
    ctx.lineCap = "round";
    ctx.stroke();
  }

  function loadImageAsUrl() {
    return new Promise((resolve, reject) => {
      let board = Board.instance;
      let fileLoader = new FileLoader('.jpg,.jpeg,.gif,.png,.svg,.bmp', {reader: 'readAsArrayBuffer'});
      fileLoader.onload = (result) => {
        if (result) {
          let blob = new Blob([result], {type: fileLoader.fileTypes[0]});
          resolve(window.URL.createObjectURL(blob));
        }
      };
      fileLoader.prompt();
    });
  }

  function remove(...idxs) {
    let adapter = Board.bgAdapter;
    let onDraw = Board.bgAdapter.onDraw;
    let onDrawCell = Board.bgAdapter.onDrawCell;
    idxs[0] || idxs.push(
      adapter.onDraw.concat(adapter.onDrawCell)
      .filter((e) => e.idx).map((e) => e.idx).sort().slice(-1)[0]
    );
    for (let idx of idxs) {
      onDraw.replace(onDraw.filter((e) => !e.idx || e.idx != idx));
      onDrawCell.replace(onDrawCell.filter((e) => !e.idx || e.idx != idx));
      Board.instance.draw();
    }
  }

  function removeAll() {
    let adapter = Board.bgAdapter;
    let idxs = adapter.onDraw.concat(adapter.onDrawCell).filter((e) => e.idx).map((e) => e.idx);
    remove(...idxs);
  }

  let examples = {
    customCodeDemos,
    hexagonFactory,
    circleFactory,
    triangleFactory,
    lineFactory,
    fillMax,
    fillMin,
    outlineMax,
    outlineMin,
    loadImageAsUrl,
    remove,
    removeAll,

    maxFilledCircle: circleFactory(fillMax),
    minFilledCircle: circleFactory(fillMin),
    maxOutlineCircle: circleFactory(outlineMax),
    minOutlineCircle: circleFactory(outlineMin),
    maxFilledTriangle: triangleFactory(fillMax),
    minFilledTriangle: triangleFactory(fillMin),
    maxOutlineTriangle: triangleFactory(outlineMax),
    minOutlineTriangle: triangleFactory(outlineMin),

    maxLine: lineFactory(outlineMax),
    minLine: lineFactory(outlineMin),

    maxFilledHexagon: hexagonFactory({fill: true}, (a, b, c) => Math.max(a.state, b.state, c.state)),
    minFilledHexagon: hexagonFactory({fill: true}, (a, b, c) => Math.min(a.state, b.state, c.state)),
    maxOutlineHexagon: hexagonFactory({stroke: true}, (a, b, c) => Math.max(a.state, b.state, c.state)),
    minOutlineHexagon: hexagonFactory({stroke: true}, (a, b, c) => Math.min(a.state, b.state, c.state)),

    drawNoise: (styles=[], fnName='drawFilledPointyHex') => {
      let model = Board.model;
      let adapter = Board.bgAdapter;
      let drawFn = adapter[fnName].bind(adapter);
      let fn = () => {
        model.cells.forEach((cell) => {
          let style = styles[Math.floor(Math.random() * styles.length)];
          drawFn(cell, style);
        });
      };
      fn.idx = ++fnCount;
      adapter.onDraw.push(fn);
      return fn.idx;
    },

    drawBackgroundImage: (url, opts={}) => {
      let fnIdx = ++fnCount;
      let adapter = Board.bgAdapter;
      (async () => {
        let defaults = {
          scale: 1,
        }
        url = url || await loadImageAsUrl();
        opts = Object.assign(defaults, opts);
        let img = new Image();
        img.src = url;
        img.onload = () => {
          let adapter = Board.bgAdapter;
          let fn = () => {
            let w = img.width;
            let h = img.height;
            let viewW = Board.instance.bg.width;
            let viewH = Board.instance.bg.height;
            let scaleAspect = Board.instance.scaleX / Board.instance.scaleY;
            if (opts.scale) {
              if (viewH < viewW) {
                w = viewW * +opts.scale;
                h = w * img.height / img.width / scaleAspect;;
              }
              else {
                h = viewH * +opts.scale;
                w = h * img.width / img.height * scaleAspect;
              }
            }
            else {
              w = w * Board.instance.scaleX;
              h = h * Board.instance.scaleY;
            }
            let x = (viewW - w) / 2;
            let y = (viewH - h) / 2;
            adapter.context.save();
            adapter.context.setTransform(1, 0, 0, 1, 0, 0);
            adapter.context.drawImage(img, x, y, w, h);
            adapter.context.restore();
          };
          fn.idx = fnIdx;
          adapter.onDraw.push(fn);
          Board.instance.draw();
        };
      })();
      return fnIdx;
    },

    drawSubtriangles: (opts) => {
      let defaults = {
        stroke: false,
        fill: false,
        skipGround: true,
        radius: null,
        offset: null,
      };
      opts = Object.assign(defaults, opts);
      let fn = function(cell) {
        if (!cell.state && opts.skipGround)
          return;
        let slice = cell.with[6].nbrSlice;
        let r = opts.radius || this.innerRadius;
        let offset = opts.offset != null ? opts.offset : this.cellBorderWidth / 2;
        for (let i = 0; i < 6; i++) {
          let state = slice[i].state;
          let j = (i + 1) % 6;
          let x = !offset ? 0 : Math.cos(Math.PI * (1 - i) / 3) * offset;
          let y = !offset ? 0 : Math.sin(Math.PI * (1 - i) / 3) * offset;
          let path = [
            [x, y],
            [x + Math.sin(Math.PI * i / 3) * r, y + Math.cos(Math.PI * i / 3) * r],
            [x + Math.sin(Math.PI * j / 3) * r, y + Math.cos(Math.PI * j / 3) * r],
          ];
          this.drawPath(cell, path);
          if (opts.stroke) {
            this.context.strokeStyle = this.strokeColors[state] || this.defaultColor;
            this.context.lineJoin = "round";
            this.context.stroke()
          }
          if (opts.fill) {
            this.context.fillStyle = this.fillColors[state] || this.defaultColor;
            this.context.fill()
          }
        }
      };
      fn.idx = ++fnCount;
      Board.bgAdapter.onDrawCell.push(fn);
      Board.instance.draw();
      return fn.idx;
    },

    drawCellImage: (url, opts={}) => {
      let fnIdx = ++fnCount;
      let adapter = Board.bgAdapter;
      (async () => {
        let defaults = {
          clip: true,
          type: Hexular.enums.TYPE_POINTY,
          scale: 1,
          states: [1],
        };
        if (!url);
        url = url || await loadImageAsUrl();
        opts = Object.assign(defaults, opts);
        let img = new Image();
        img.src = url;
        img.onload = () => {
          let w = img.width;
          let h = img.height;
          if (opts.scale) {
            w = adapter.innerRadius * 2 * +opts.scale;
            h = w * img.height / img.width;
          }
          let pathScale = opts.scale || 1;
          let path = opts.type == Hexular.enums.TYPE_POINTY
            ? Hexular.math.scalarOp(Hexular.math.vertices.map(([x, y]) => [y, x]), adapter.innerRadius * pathScale)
            : Hexular.math.scalarOp(Hexular.math.vertices, adapter.innerRadius * pathScale)
          let fn = (cell) => {
            if (!opts.states.includes(cell.state))
              return;
            let [x, y] = Board.model.cellMap.get(cell);
            x -= w / 2;
            y -= h / 2;
            adapter.context.save();
            if (opts.clip) {
              adapter.drawPath(cell, path);
              adapter.context.clip();
            }
            adapter.context.drawImage(img, 0, 0, img.width, img.height, x, y, w, h);
            adapter.context.restore();
          };
          fn.idx = ++fnCount;
          adapter.onDrawCell.push(fn);
          Board.instance.draw();
        }
      })();
      return fnIdx;
    },

    rotateColors: () => {
      Board.instance.addHook('step', () => {
        let colors = Board.bgAdapter.fillColors.slice();
        colors = colors.concat(colors.splice(1,1));
        Board.bgAdapter.fillColors = colors;
      });
    },
  }

  return examples;
})();
const Rules = (() => {
  const coreRules = Hexular.rules;
  const customRules = {
    binary1: (cell) => cell.count == 1 ? 1 : 0,

    binary2: (cell) => cell.count == 2 ? 1 : 0,

    binary3: (cell) => cell.count == 3 ? 1 : 0,

    binary12: (cell) => {
      let count = cell.count;
      return count == 1 || count == 2 ? 1 : 0;
    },

    binary23: (cell) => {
      const count = cell.count;
      return count == 2 || count == 3 ? 1 : 0;
    },

    binary34: (cell) => {
      const count = cell.count;
      return count == 3 || count == 4 ? 1 : 0;
    },

    stepDown: (cell) => cell.state - 1,

    stepUp: (cell) => cell.state + 1,

    xorCount: (cell) => cell.count % 2,

    xorTotal: (cell) => cell.total % 2,

    average: (cell) => cell.average,

    count: (cell) => cell.count,

    total: (cell) => cell.total,

    min: (cell) => cell.min,

    max: (cell) => cell.max,

    ennead: Hexular.util.ruleBuilder([
      0b001001,
      0b010010,
      0b100100,
      0b000011,
      0b000110,
      0b001100,
      0b011000,
      0b110000,
      0b100001,
    ]),

    enneadPlus: Hexular.util.ruleBuilder([
      0b001001,
      0b010010,
      0b100100,
      0b000011,
      0b000110,
      0b001100,
      0b011000,
      0b110000,
      0b100001,
    ], {miss: -1, match: 1, missRel: true, matchRel: true, rel: true}),

    expander: Hexular.util.ruleBuilder([
      0b000001,
      0b000010,
      0b000011,
      0b000100,
      0b000110,
      0b000111,
      0b001000,
      0b001100,
      0b001110,
      0b001111,
      0b010000,
      0b011000,
      0b011100,
      0b011110,
      0b100000,
      0b100001,
      0b100011,
      0b100111,
      0b110000,
      0b110001,
      0b110011,
      0b111000,
      0b111001,
      0b111100,
      0b111111,
    ], {"miss":-1,"match":1,"missRel":1,"matchRel":1,"rel":0}),

    fractalLeft: Hexular.util.ruleBuilder([
      0b010000,
      0b000100,
      0b000001,
    ]),

    fractalRight: Hexular.util.ruleBuilder([
      0b100000,
      0b001000,
      0b000010,
    ]),

    lineFilter: Hexular.util.ruleBuilder([
      0b000000,
      0b001001,
      0b010010,
      0b100100,
    ], {miss: 1, match: 0}),

    retractor: Hexular.util.ruleBuilder([
      0b000111,
      0b001110,
      0b011100,
      0b111000,
      0b110001,
      0b100011,
      0b100111,
      0b001111,
      0b011110,
      0b111100,
      0b111001,
      0b110011,
    ], {"miss":0,"match":-1,"missRel":1,"matchRel":1,"rel":0}),

    uncial: Hexular.util.ruleBuilder([
      0b000011,
      0b000110,
      0b000111,
      0b001100,
      0b001110,
      0b011000,
      0b011100,
      0b100001,
      0b100011,
      0b110000,
      0b110001,
      0b111000,
    ], {"miss":-1,"match":1,"missRel":1,"matchRel":1,"rel":1}),

    fancytown: (cell) => {
      const tot = cell.total;
      if (tot > 2 && tot < 5)
        return cell.state + 1;
      else if (tot >= 9)
        return cell.state - 1;
      else
        return cell.state;
    },
  };
  let entries = Object.entries(customRules);
  entries.sort((a, b) => {
    let strOrder = a[0].localeCompare(b[0]);
    let [an, bn] = [a[1], b[1]].map((e) => e.n != null ? 1 : -1);
    return an ^ bn == -2 ? an - bn : strOrder;
  });
  let rules = Object.assign({}, coreRules, Config.toObject(entries));
  return rules;
})();
const Presets = {
  default: new Preset({filters: {deltaFilter: true}}, Array(12).fill('ennead')),

  enneadPlus: new Preset({filters: {clipBottomFilter: true, modFilter: true, edgeFilter: true}}, Array(12).fill('enneadPlus')),

  gliderWorld: new Preset({filters: {edgeFilter: true}}, [
    'ennead',
    'ennead',
  ]),

  rainbowRoad: new Preset(Object.assign(Array(12).fill('stepUp'), ['fractalLeft'])),

  fancytownClassic: new Preset({nh: 19}, Array(12).fill('fancytown')),

  grayGoo: new Preset({nh: 19}, Object.assign(Array(10).fill('average'), ['total', 'total'])),
};
const Themes = (() => {
  // Original 2017 color palette
  let classicColors = Object.assign([], Hexular.DEFAULTS.colors, [
    'transparent',
    '#cccccc',
    '#999999',
    '#666666',
    '#333333',
    '#cc4444',
    '#ee7722',
    '#eebb33',
    '#66bb33',
    '#66aaaa',
    '#4455bb',
    '#aa55bb',
  ]);

  let rainbow = Object.assign([], Hexular.DEFAULTS.colors, [
    'transparent',
    '#ff0000',
    '#ffaa00',
    '#aaff00',
    '#00ff00',
    '#00ffff',
    '#00aaff',
    '#0066ff',
    '#0000ff',
    '#aa00ff',
    '#ff00ff',
    '#ff00aa',
  ]);

  let themes = {
    light: {
    },
    smooth: {
      cellGap: -0.5,
    },
    mango: {
      colors: [
        null,
        null,
        null,
        null,
        null,
        '#cc5555',
        '#ef9f00',
        '#eedd00',
        '#6fbf44',
        '#33cccc',
        '#3366ee',
        '#cc33ee',
      ],
    },
    beige: {
      modelBackground: '#efefe7',
    },
    beigeBlobular: {
      modelBackground: '#efefe7',
      cellGap: -12.75,
    },
    classic: {
      colors: classicColors,
    },
    white: {
      pageBackground: '#ffffff',
      colors: Hexular.DEFAULTS.colors.slice(),
    },
    lightRainbow: {
      cellGap: -0.5,
      colors: rainbow,
    },
    vaporRainbow : {
      cellGap: -0.5,
      pageBackground: '#ffffff',
      modelBackground: '#fffff7',
      colors: Config.merge(classicColors, [
        null,
        '#f7f7ef33',
        '#efefe766',
        '#e7e7df99',
        '#ff0000',
        '#ffaa00',
        '#aaff00',
        '#00ff00',
        '#00ffff',
        '#00aaff',
        '#9900ff',
        '#ff0099',
      ]),
    },
    beigeRainbow: {
      pageBackground: '#ffffff',
      modelBackground: '#fafafa',
      colors: [
        null,
        '#ccccbb',
        '#ffaa11',
        '#ffcc22',
        '#aadd11',
        '#11cccc',
        '#1188ff',
        '#cc44ff',
        '#ff44bb',
        '#cc3333',
        '#aaaa33',
        '#332211',
      ],
    },
    dark: {
      pageBackground: '#111111',
      modelBackground: '#000000',
      cellGap: -0.5,
      colors: Config.merge([], classicColors, [
        null,
        '#888888',
        '#aaaaaa',
        '#cccccc',
        '#eeeeee',
      ]),
    },
    darkRainbow: {
      pageBackground: '#111111',
      modelBackground: '#000000',
      cellGap: -0.5,
      colors: rainbow,
    },
    darkRainbow2: {
      pageBackground: '#111111',
      modelBackground: '#000000',
      cellGap: 8,
      colors: [
        null,
        '#ff2d00',
        '#ff7708',
        '#ffd400',
        '#e4ff00',
        '#5cff00',
        '#00eeff',
        '#0080ff',
        '#0044ff',
        '#dd00ff',
        '#ff00cc',
        '#ffffff',
      ],
    },
    smoothChalkRainbow: {
      pageBackground: '#111111',
      modelBackground: '#001122',
      cellGap: -13,
      cellBorderWidth: 2,
      colors: [
        null,
        '#664466',
        '#dd2200',
        '#ffaa00',
        '#ddee00',
        '#00ee00',
        '#00dddd',
        '#00aaff',
        '#0066cc',
        '#3333ff',
        '#dd00dd',
        '#eeeedd',
      ],
    }
  };
  return themes;
})();
/**
 * jscolor - JavaScript Color Picker
 *
 * @link    http://jscolor.com
 * @license For open source use: GPLv3
 *          For commercial use: JSColor Commercial License
 * @author  Jan Odvarko
 * @version 2.0.5
 *
 * See usage examples at http://jscolor.com/examples/
 */

 /* Modified 2020 for use with Hexular for stylistic consistency by Graham Steele */


"use strict";


if (!window.jscolor) { window.jscolor = (function () {


var jsc = {


  register : function () {
    jsc.attachDOMReadyEvent(jsc.init);
    jsc.attachEvent(document, 'mousedown', jsc.onDocumentMouseDown);
    jsc.attachEvent(document, 'touchstart', jsc.onDocumentTouchStart);
    jsc.attachEvent(window, 'resize', jsc.onWindowResize);
  },


  init : function () {
    if (jsc.jscolor.lookupClass) {
      jsc.jscolor.installByClassName(jsc.jscolor.lookupClass);
    }
  },


  tryInstallOnElements : function (elms, className) {
    var matchClass = new RegExp('(^|\\s)(' + className + ')(\\s*(\\{[^}]*\\})|\\s|$)', 'i');

    for (var i = 0; i < elms.length; i += 1) {
      if (elms[i].type !== undefined && elms[i].type.toLowerCase() == 'color') {
        if (jsc.isColorAttrSupported) {
          // skip inputs of type 'color' if supported by the browser
          continue;
        }
      }
      var m;
      if (!elms[i].jscolor && elms[i].className && (m = elms[i].className.match(matchClass))) {
        var targetElm = elms[i];
        var optsStr = null;

        var dataOptions = jsc.getDataAttr(targetElm, 'jscolor');
        if (dataOptions !== null) {
          optsStr = dataOptions;
        } else if (m[4]) {
          optsStr = m[4];
        }

        var opts = {};
        if (optsStr) {
          try {
            opts = (new Function ('return (' + optsStr + ')'))();
          } catch(eParseError) {
            jsc.warn('Error parsing jscolor options: ' + eParseError + ':\n' + optsStr);
          }
        }
        targetElm.jscolor = new jsc.jscolor(targetElm, opts);
      }
    }
  },


  isColorAttrSupported : (function () {
    var elm = document.createElement('input');
    if (elm.setAttribute) {
      elm.setAttribute('type', 'color');
      if (elm.type.toLowerCase() == 'color') {
        return true;
      }
    }
    return false;
  })(),


  isCanvasSupported : (function () {
    var elm = document.createElement('canvas');
    return !!(elm.getContext && elm.getContext('2d'));
  })(),


  fetchElement : function (mixed) {
    return typeof mixed === 'string' ? document.getElementById(mixed) : mixed;
  },


  isElementType : function (elm, type) {
    return elm.nodeName.toLowerCase() === type.toLowerCase();
  },


  getDataAttr : function (el, name) {
    var attrName = 'data-' + name;
    var attrValue = el.getAttribute(attrName);
    if (attrValue !== null) {
      return attrValue;
    }
    return null;
  },


  attachEvent : function (el, evnt, func) {
    if (el.addEventListener) {
      el.addEventListener(evnt, func, false);
    } else if (el.attachEvent) {
      el.attachEvent('on' + evnt, func);
    }
  },


  detachEvent : function (el, evnt, func) {
    if (el.removeEventListener) {
      el.removeEventListener(evnt, func, false);
    } else if (el.detachEvent) {
      el.detachEvent('on' + evnt, func);
    }
  },


  _attachedGroupEvents : {},


  attachGroupEvent : function (groupName, el, evnt, func) {
    if (!jsc._attachedGroupEvents.hasOwnProperty(groupName)) {
      jsc._attachedGroupEvents[groupName] = [];
    }
    jsc._attachedGroupEvents[groupName].push([el, evnt, func]);
    jsc.attachEvent(el, evnt, func);
  },


  detachGroupEvents : function (groupName) {
    if (jsc._attachedGroupEvents.hasOwnProperty(groupName)) {
      for (var i = 0; i < jsc._attachedGroupEvents[groupName].length; i += 1) {
        var evt = jsc._attachedGroupEvents[groupName][i];
        jsc.detachEvent(evt[0], evt[1], evt[2]);
      }
      delete jsc._attachedGroupEvents[groupName];
    }
  },


  attachDOMReadyEvent : function (func) {
    var fired = false;
    var fireOnce = function () {
      if (!fired) {
        fired = true;
        func();
      }
    };

    if (document.readyState === 'complete') {
      setTimeout(fireOnce, 1); // async
      return;
    }

    if (document.addEventListener) {
      document.addEventListener('DOMContentLoaded', fireOnce, false);

      // Fallback
      window.addEventListener('load', fireOnce, false);

    } else if (document.attachEvent) {
      // IE
      document.attachEvent('onreadystatechange', function () {
        if (document.readyState === 'complete') {
          document.detachEvent('onreadystatechange', arguments.callee);
          fireOnce();
        }
      })

      // Fallback
      window.attachEvent('onload', fireOnce);

      // IE7/8
      if (document.documentElement.doScroll && window == window.top) {
        var tryScroll = function () {
          if (!document.body) { return; }
          try {
            document.documentElement.doScroll('left');
            fireOnce();
          } catch (e) {
            setTimeout(tryScroll, 1);
          }
        };
        tryScroll();
      }
    }
  },


  warn : function (msg) {
    if (window.console && window.console.warn) {
      window.console.warn(msg);
    }
  },


  preventDefault : function (e) {
    if (e.preventDefault) { e.preventDefault(); }
    e.returnValue = false;
  },


  captureTarget : function (target) {
    // IE
    if (target.setCapture) {
      jsc._capturedTarget = target;
      jsc._capturedTarget.setCapture();
    }
  },


  releaseTarget : function () {
    // IE
    if (jsc._capturedTarget) {
      jsc._capturedTarget.releaseCapture();
      jsc._capturedTarget = null;
    }
  },


  fireEvent : function (el, evnt) {
    if (!el) {
      return;
    }
    if (document.createEvent) {
      var ev = document.createEvent('HTMLEvents');
      ev.initEvent(evnt, true, true);
      el.dispatchEvent(ev);
    } else if (document.createEventObject) {
      var ev = document.createEventObject();
      el.fireEvent('on' + evnt, ev);
    } else if (el['on' + evnt]) { // alternatively use the traditional event model
      el['on' + evnt]();
    }
  },


  classNameToList : function (className) {
    return className.replace(/^\s+|\s+$/g, '').split(/\s+/);
  },


  // The className parameter (str) can only contain a single class name
  hasClass : function (elm, className) {
    if (!className) {
      return false;
    }
    return -1 != (' ' + elm.className.replace(/\s+/g, ' ') + ' ').indexOf(' ' + className + ' ');
  },


  // The className parameter (str) can contain multiple class names separated by whitespace
  setClass : function (elm, className) {
    var classList = jsc.classNameToList(className);
    for (var i = 0; i < classList.length; i += 1) {
      if (!jsc.hasClass(elm, classList[i])) {
        elm.className += (elm.className ? ' ' : '') + classList[i];
      }
    }
  },


  // The className parameter (str) can contain multiple class names separated by whitespace
  unsetClass : function (elm, className) {
    var classList = jsc.classNameToList(className);
    for (var i = 0; i < classList.length; i += 1) {
      var repl = new RegExp(
        '^\\s*' + classList[i] + '\\s*|' +
        '\\s*' + classList[i] + '\\s*$|' +
        '\\s+' + classList[i] + '(\\s+)',
        'g'
      );
      elm.className = elm.className.replace(repl, '$1');
    }
  },


  getStyle : function (elm) {
    return window.getComputedStyle ? window.getComputedStyle(elm) : elm.currentStyle;
  },


  setStyle : (function () {
    var helper = document.createElement('div');
    var getSupportedProp = function (names) {
      for (var i = 0; i < names.length; i += 1) {
        if (names[i] in helper.style) {
          return names[i];
        }
      }
    };
    var props = {
      borderRadius: getSupportedProp(['borderRadius', 'MozBorderRadius', 'webkitBorderRadius']),
      boxShadow: getSupportedProp(['boxShadow', 'MozBoxShadow', 'webkitBoxShadow'])
    };
    return function (elm, prop, value) {
      switch (prop.toLowerCase()) {
      case 'opacity':
        var alphaOpacity = Math.round(parseFloat(value) * 100);
        elm.style.opacity = value;
        elm.style.filter = 'alpha(opacity=' + alphaOpacity + ')';
        break;
      default:
        elm.style[props[prop]] = value;
        break;
      }
    };
  })(),


  setBorderRadius : function (elm, value) {
    jsc.setStyle(elm, 'borderRadius', value || '0');
  },


  setBoxShadow : function (elm, value) {
    jsc.setStyle(elm, 'boxShadow', value || 'none');
  },


  getElementPos : function (e, relativeToViewport) {
    var x=0, y=0;
    var rect = e.getBoundingClientRect();
    x = rect.left;
    y = rect.top;
    if (!relativeToViewport) {
      var viewPos = jsc.getViewPos();
      x += viewPos[0];
      y += viewPos[1];
    }
    return [x, y];
  },


  getElementSize : function (e) {
    return [e.offsetWidth, e.offsetHeight];
  },


  // get pointer's X/Y coordinates relative to viewport
  getAbsPointerPos : function (e) {
    if (!e) { e = window.event; }
    var x = 0, y = 0;
    if (typeof e.changedTouches !== 'undefined' && e.changedTouches.length) {
      // touch devices
      x = e.changedTouches[0].clientX;
      y = e.changedTouches[0].clientY;
    } else if (typeof e.clientX === 'number') {
      x = e.clientX;
      y = e.clientY;
    }
    return { x: x, y: y };
  },


  // get pointer's X/Y coordinates relative to target element
  getRelPointerPos : function (e) {
    if (!e) { e = window.event; }
    var target = e.target || e.srcElement;
    var targetRect = target.getBoundingClientRect();

    var x = 0, y = 0;

    var clientX = 0, clientY = 0;
    if (typeof e.changedTouches !== 'undefined' && e.changedTouches.length) {
      // touch devices
      clientX = e.changedTouches[0].clientX;
      clientY = e.changedTouches[0].clientY;
    } else if (typeof e.clientX === 'number') {
      clientX = e.clientX;
      clientY = e.clientY;
    }

    x = clientX - targetRect.left;
    y = clientY - targetRect.top;
    return { x: x, y: y };
  },


  getViewPos : function () {
    var doc = document.documentElement;
    return [
      (window.pageXOffset || doc.scrollLeft) - (doc.clientLeft || 0),
      (window.pageYOffset || doc.scrollTop) - (doc.clientTop || 0)
    ];
  },


  getViewSize : function () {
    var doc = document.documentElement;
    return [
      (window.innerWidth || doc.clientWidth),
      (window.innerHeight || doc.clientHeight),
    ];
  },


  redrawPosition : function () {

    if (jsc.picker && jsc.picker.owner) {
      var thisObj = jsc.picker.owner;

      var tp, vp;

      if (thisObj.fixed) {
        // Fixed elements are positioned relative to viewport,
        // therefore we can ignore the scroll offset
        tp = jsc.getElementPos(thisObj.targetElement, true); // target pos
        vp = [0, 0]; // view pos
      } else {
        tp = jsc.getElementPos(thisObj.targetElement); // target pos
        vp = jsc.getViewPos(); // view pos
      }

      var ts = jsc.getElementSize(thisObj.targetElement); // target size
      var vs = jsc.getViewSize(); // view size
      var ps = jsc.getPickerOuterDims(thisObj); // picker size
      var a, b, c;
      switch (thisObj.position.toLowerCase()) {
        case 'left': a=1; b=0; c=-1; break;
        case 'right':a=1; b=0; c=1; break;
        case 'top':  a=0; b=1; c=-1; break;
        default:     a=0; b=1; c=1; break;
      }
      var l = (ts[b]+ps[b])/2;

      // compute picker position
      if (!thisObj.smartPosition) {
        var pp = [
          tp[a],
          tp[b]+ts[b]-l+l*c
        ];
      } else {
        var pp = [
          -vp[a]+tp[a]+ps[a] > vs[a] ?
            (-vp[a]+tp[a]+ts[a]/2 > vs[a]/2 && tp[a]+ts[a]-ps[a] >= 0 ? tp[a]+ts[a]-ps[a] : tp[a]) :
            tp[a],
          -vp[b]+tp[b]+ts[b]+ps[b]-l+l*c > vs[b] ?
            (-vp[b]+tp[b]+ts[b]/2 > vs[b]/2 && tp[b]+ts[b]-l-l*c >= 0 ? tp[b]+ts[b]-l-l*c : tp[b]+ts[b]-l+l*c) :
            (tp[b]+ts[b]-l+l*c >= 0 ? tp[b]+ts[b]-l+l*c : tp[b]+ts[b]-l-l*c)
        ];
      }

      var x = pp[a];
      var y = pp[b];
      var positionValue = thisObj.fixed ? 'fixed' : 'absolute';
      var contractShadow =
        (pp[0] + ps[0] > tp[0] || pp[0] < tp[0] + ts[0]) &&
        (pp[1] + ps[1] < tp[1] + ts[1]);

      jsc._drawPosition(thisObj, x, y, positionValue, contractShadow);
    }
  },


  _drawPosition : function (thisObj, x, y, positionValue, contractShadow) {
    var vShadow = contractShadow ? 0 : thisObj.shadowBlur; // px

    jsc.picker.wrap.style.position = positionValue;
    jsc.picker.wrap.style.left = x + 'px';
    jsc.picker.wrap.style.top = y + 'px';

    jsc.setBoxShadow(
      jsc.picker.boxS,
      thisObj.shadow ?
        new jsc.BoxShadow(0, vShadow, thisObj.shadowBlur, 0, thisObj.shadowColor) :
        null);
  },


  getPickerDims : function (thisObj) {
    var displaySlider = !!jsc.getSliderComponent(thisObj);
    var dims = [
      2 * thisObj.insetWidth + 2 * thisObj.padding + thisObj.width +
        (displaySlider ? 2 * thisObj.insetWidth + jsc.getPadToSliderPadding(thisObj) + thisObj.sliderSize : 0),
      2 * thisObj.insetWidth + 2 * thisObj.padding + thisObj.height +
        (thisObj.closable ? 2 * thisObj.insetWidth + thisObj.padding + thisObj.buttonHeight : 0)
    ];
    return dims;
  },


  getPickerOuterDims : function (thisObj) {
    var dims = jsc.getPickerDims(thisObj);
    return [
      dims[0] + 2 * thisObj.borderWidth,
      dims[1] + 2 * thisObj.borderWidth
    ];
  },


  getPadToSliderPadding : function (thisObj) {
    return Math.max(thisObj.padding, 1.5 * (2 * thisObj.pointerBorderWidth + thisObj.pointerThickness));
  },


  getPadYComponent : function (thisObj) {
    switch (thisObj.mode.charAt(1).toLowerCase()) {
      case 'v': return 'v'; break;
    }
    return 's';
  },


  getSliderComponent : function (thisObj) {
    if (thisObj.mode.length > 2) {
      switch (thisObj.mode.charAt(2).toLowerCase()) {
        case 's': return 's'; break;
        case 'v': return 'v'; break;
      }
    }
    return null;
  },


  onDocumentMouseDown : function (e) {
    if (!e) { e = window.event; }
    var target = e.target || e.srcElement;

    if (target._jscLinkedInstance) {
      if (target._jscLinkedInstance.showOnClick) {
        target._jscLinkedInstance.show();
      }
    } else if (target._jscControlName) {
      jsc.onControlPointerStart(e, target, target._jscControlName, 'mouse');
    } else {
      // Mouse is outside the picker controls -> hide the color picker!
      if (jsc.picker && jsc.picker.owner) {
        jsc.picker.owner.hide();
      }
    }
  },


  onDocumentTouchStart : function (e) {
    if (!e) { e = window.event; }
    var target = e.target || e.srcElement;

    if (target._jscLinkedInstance) {
      if (target._jscLinkedInstance.showOnClick) {
        target._jscLinkedInstance.show();
      }
    } else if (target._jscControlName) {
      jsc.onControlPointerStart(e, target, target._jscControlName, 'touch');
    } else {
      if (jsc.picker && jsc.picker.owner) {
        jsc.picker.owner.hide();
      }
    }
  },


  onWindowResize : function (e) {
    jsc.redrawPosition();
  },


  onParentScroll : function (e) {
    // hide the picker when one of the parent elements is scrolled
    if (jsc.picker && jsc.picker.owner) {
      jsc.picker.owner.hide();
    }
  },


  _pointerMoveEvent : {
    mouse: 'mousemove',
    touch: 'touchmove'
  },
  _pointerEndEvent : {
    mouse: 'mouseup',
    touch: 'touchend'
  },


  _pointerOrigin : null,
  _capturedTarget : null,


  onControlPointerStart : function (e, target, controlName, pointerType) {
    var thisObj = target._jscInstance;

    jsc.preventDefault(e);
    jsc.captureTarget(target);

    var registerDragEvents = function (doc, offset) {
      jsc.attachGroupEvent('drag', doc, jsc._pointerMoveEvent[pointerType],
        jsc.onDocumentPointerMove(e, target, controlName, pointerType, offset));
      jsc.attachGroupEvent('drag', doc, jsc._pointerEndEvent[pointerType],
        jsc.onDocumentPointerEnd(e, target, controlName, pointerType));
    };

    registerDragEvents(document, [0, 0]);

    if (window.parent && window.frameElement) {
      var rect = window.frameElement.getBoundingClientRect();
      var ofs = [-rect.left, -rect.top];
      registerDragEvents(window.parent.window.document, ofs);
    }

    var abs = jsc.getAbsPointerPos(e);
    var rel = jsc.getRelPointerPos(e);
    jsc._pointerOrigin = {
      x: abs.x - rel.x,
      y: abs.y - rel.y
    };

    switch (controlName) {
    case 'pad':
      // if the slider is at the bottom, move it up
      switch (jsc.getSliderComponent(thisObj)) {
      case 's': if (thisObj.hsv[1] === 0) { thisObj.fromHSV(null, 100, null); }; break;
      case 'v': if (thisObj.hsv[2] === 0) { thisObj.fromHSV(null, null, 100); }; break;
      }
      jsc.setPad(thisObj, e, 0, 0);
      break;

    case 'sld':
      jsc.setSld(thisObj, e, 0);
      break;
    }

    jsc.dispatchFineChange(thisObj);
  },


  onDocumentPointerMove : function (e, target, controlName, pointerType, offset) {
    return function (e) {
      var thisObj = target._jscInstance;
      switch (controlName) {
      case 'pad':
        if (!e) { e = window.event; }
        jsc.setPad(thisObj, e, offset[0], offset[1]);
        jsc.dispatchFineChange(thisObj);
        break;

      case 'sld':
        if (!e) { e = window.event; }
        jsc.setSld(thisObj, e, offset[1]);
        jsc.dispatchFineChange(thisObj);
        break;
      }
    }
  },


  onDocumentPointerEnd : function (e, target, controlName, pointerType) {
    return function (e) {
      var thisObj = target._jscInstance;
      jsc.detachGroupEvents('drag');
      jsc.releaseTarget();
      // Always dispatch changes after detaching outstanding mouse handlers,
      // in case some user interaction will occur in user's onchange callback
      // that would intrude with current mouse events
      jsc.dispatchChange(thisObj);
    };
  },


  dispatchChange : function (thisObj) {
    if (thisObj.valueElement) {
      if (jsc.isElementType(thisObj.valueElement, 'input')) {
        jsc.fireEvent(thisObj.valueElement, 'change');
      }
    }
  },


  dispatchFineChange : function (thisObj) {
    if (thisObj.onFineChange) {
      var callback;
      if (typeof thisObj.onFineChange === 'string') {
        callback = new Function (thisObj.onFineChange);
      } else {
        callback = thisObj.onFineChange;
      }
      callback.call(thisObj);
    }
  },


  setPad : function (thisObj, e, ofsX, ofsY) {
    var pointerAbs = jsc.getAbsPointerPos(e);
    var x = ofsX + pointerAbs.x - jsc._pointerOrigin.x - thisObj.padding - thisObj.insetWidth;
    var y = ofsY + pointerAbs.y - jsc._pointerOrigin.y - thisObj.padding - thisObj.insetWidth;

    var xVal = x * (360 / (thisObj.width - 1));
    var yVal = 100 - (y * (100 / (thisObj.height - 1)));

    switch (jsc.getPadYComponent(thisObj)) {
    case 's': thisObj.fromHSV(xVal, yVal, null, jsc.leaveSld); break;
    case 'v': thisObj.fromHSV(xVal, null, yVal, jsc.leaveSld); break;
    }
  },


  setSld : function (thisObj, e, ofsY) {
    var pointerAbs = jsc.getAbsPointerPos(e);
    var y = ofsY + pointerAbs.y - jsc._pointerOrigin.y - thisObj.padding - thisObj.insetWidth;

    var yVal = 100 - (y * (100 / (thisObj.height - 1)));

    switch (jsc.getSliderComponent(thisObj)) {
    case 's': thisObj.fromHSV(null, yVal, null, jsc.leavePad); break;
    case 'v': thisObj.fromHSV(null, null, yVal, jsc.leavePad); break;
    }
  },


  _vmlNS : 'jsc_vml_',
  _vmlCSS : 'jsc_vml_css_',
  _vmlReady : false,


  initVML : function () {
    if (!jsc._vmlReady) {
      // init VML namespace
      var doc = document;
      if (!doc.namespaces[jsc._vmlNS]) {
        doc.namespaces.add(jsc._vmlNS, 'urn:schemas-microsoft-com:vml');
      }
      if (!doc.styleSheets[jsc._vmlCSS]) {
        var tags = ['shape', 'shapetype', 'group', 'background', 'path', 'formulas', 'handles', 'fill', 'stroke', 'shadow', 'textbox', 'textpath', 'imagedata', 'line', 'polyline', 'curve', 'rect', 'roundrect', 'oval', 'arc', 'image'];
        var ss = doc.createStyleSheet();
        ss.owningElement.id = jsc._vmlCSS;
        for (var i = 0; i < tags.length; i += 1) {
          ss.addRule(jsc._vmlNS + '\\:' + tags[i], 'behavior:url(#default#VML);');
        }
      }
      jsc._vmlReady = true;
    }
  },


  createPalette : function () {

    var paletteObj = {
      elm: null,
      draw: null
    };

    if (jsc.isCanvasSupported) {
      // Canvas implementation for modern browsers

      var canvas = document.createElement('canvas');
      var ctx = canvas.getContext('2d');

      var drawFunc = function (width, height, type) {
        canvas.width = width;
        canvas.height = height;

        ctx.clearRect(0, 0, canvas.width, canvas.height);

        var hGrad = ctx.createLinearGradient(0, 0, canvas.width, 0);
        hGrad.addColorStop(0 / 6, '#F00');
        hGrad.addColorStop(1 / 6, '#FF0');
        hGrad.addColorStop(2 / 6, '#0F0');
        hGrad.addColorStop(3 / 6, '#0FF');
        hGrad.addColorStop(4 / 6, '#00F');
        hGrad.addColorStop(5 / 6, '#F0F');
        hGrad.addColorStop(6 / 6, '#F00');

        ctx.fillStyle = hGrad;
        ctx.fillRect(0, 0, canvas.width, canvas.height);

        var vGrad = ctx.createLinearGradient(0, 0, 0, canvas.height);
        switch (type.toLowerCase()) {
        case 's':
          vGrad.addColorStop(0, 'rgba(255,255,255,0)');
          vGrad.addColorStop(1, 'rgba(255,255,255,1)');
          break;
        case 'v':
          vGrad.addColorStop(0, 'rgba(0,0,0,0)');
          vGrad.addColorStop(1, 'rgba(0,0,0,1)');
          break;
        }
        ctx.fillStyle = vGrad;
        ctx.fillRect(0, 0, canvas.width, canvas.height);
      };

      paletteObj.elm = canvas;
      paletteObj.draw = drawFunc;

    } else {
      // VML fallback for IE 7 and 8

      jsc.initVML();

      var vmlContainer = document.createElement('div');
      vmlContainer.style.position = 'relative';
      vmlContainer.style.overflow = 'hidden';

      var hGrad = document.createElement(jsc._vmlNS + ':fill');
      hGrad.type = 'gradient';
      hGrad.method = 'linear';
      hGrad.angle = '90';
      hGrad.colors = '16.67% #F0F, 33.33% #00F, 50% #0FF, 66.67% #0F0, 83.33% #FF0'

      var hRect = document.createElement(jsc._vmlNS + ':rect');
      hRect.style.position = 'absolute';
      hRect.style.left = -1 + 'px';
      hRect.style.top = -1 + 'px';
      hRect.stroked = false;
      hRect.appendChild(hGrad);
      vmlContainer.appendChild(hRect);

      var vGrad = document.createElement(jsc._vmlNS + ':fill');
      vGrad.type = 'gradient';
      vGrad.method = 'linear';
      vGrad.angle = '180';
      vGrad.opacity = '0';

      var vRect = document.createElement(jsc._vmlNS + ':rect');
      vRect.style.position = 'absolute';
      vRect.style.left = -1 + 'px';
      vRect.style.top = -1 + 'px';
      vRect.stroked = false;
      vRect.appendChild(vGrad);
      vmlContainer.appendChild(vRect);

      var drawFunc = function (width, height, type) {
        vmlContainer.style.width = width + 'px';
        vmlContainer.style.height = height + 'px';

        hRect.style.width =
        vRect.style.width =
          (width + 1) + 'px';
        hRect.style.height =
        vRect.style.height =
          (height + 1) + 'px';

        // Colors must be specified during every redraw, otherwise IE won't display
        // a full gradient during a subsequential redraw
        hGrad.color = '#F00';
        hGrad.color2 = '#F00';

        switch (type.toLowerCase()) {
        case 's':
          vGrad.color = vGrad.color2 = '#FFF';
          break;
        case 'v':
          vGrad.color = vGrad.color2 = '#000';
          break;
        }
      };

      paletteObj.elm = vmlContainer;
      paletteObj.draw = drawFunc;
    }

    return paletteObj;
  },


  createSliderGradient : function () {

    var sliderObj = {
      elm: null,
      draw: null
    };

    if (jsc.isCanvasSupported) {
      // Canvas implementation for modern browsers

      var canvas = document.createElement('canvas');
      var ctx = canvas.getContext('2d');

      var drawFunc = function (width, height, color1, color2) {
        canvas.width = width;
        canvas.height = height;

        ctx.clearRect(0, 0, canvas.width, canvas.height);

        var grad = ctx.createLinearGradient(0, 0, 0, canvas.height);
        grad.addColorStop(0, color1);
        grad.addColorStop(1, color2);

        ctx.fillStyle = grad;
        ctx.fillRect(0, 0, canvas.width, canvas.height);
      };

      sliderObj.elm = canvas;
      sliderObj.draw = drawFunc;

    } else {
      // VML fallback for IE 7 and 8

      jsc.initVML();

      var vmlContainer = document.createElement('div');
      vmlContainer.style.position = 'relative';
      vmlContainer.style.overflow = 'hidden';

      var grad = document.createElement(jsc._vmlNS + ':fill');
      grad.type = 'gradient';
      grad.method = 'linear';
      grad.angle = '180';

      var rect = document.createElement(jsc._vmlNS + ':rect');
      rect.style.position = 'absolute';
      rect.style.left = -1 + 'px';
      rect.style.top = -1 + 'px';
      rect.stroked = false;
      rect.appendChild(grad);
      vmlContainer.appendChild(rect);

      var drawFunc = function (width, height, color1, color2) {
        vmlContainer.style.width = width + 'px';
        vmlContainer.style.height = height + 'px';

        rect.style.width = (width + 1) + 'px';
        rect.style.height = (height + 1) + 'px';

        grad.color = color1;
        grad.color2 = color2;
      };

      sliderObj.elm = vmlContainer;
      sliderObj.draw = drawFunc;
    }

    return sliderObj;
  },


  leaveValue : 1<<0,
  leaveStyle : 1<<1,
  leavePad : 1<<2,
  leaveSld : 1<<3,


  BoxShadow : (function () {
    var BoxShadow = function (hShadow, vShadow, blur, spread, color, inset) {
      this.hShadow = hShadow;
      this.vShadow = vShadow;
      this.blur = blur;
      this.spread = spread;
      this.color = color;
      this.inset = !!inset;
    };

    BoxShadow.prototype.toString = function () {
      var vals = [
        Math.round(this.hShadow) + 'px',
        Math.round(this.vShadow) + 'px',
        Math.round(this.blur) + 'px',
        Math.round(this.spread) + 'px',
        this.color
      ];
      if (this.inset) {
        vals.push('inset');
      }
      return vals.join(' ');
    };

    return BoxShadow;
  })(),


  //
  // Usage:
  // var myColor = new jscolor(<targetElement> [, <options>])
  //

  jscolor : function (targetElement, options) {

    // General options
    //
    this.value = null; // initial HEX color. To change it later, use methods fromString(), fromHSV() and fromRGB()
    this.valueElement = targetElement; // element that will be used to display and input the color code
    this.styleElement = targetElement; // element that will preview the picked color using CSS backgroundColor
    this.required = true; // whether the associated text <input> can be left empty
    this.refine = false; // whether to refine the entered color code (e.g. uppercase it and remove whitespace)
    this.hash = true; // whether to prefix the HEX color code with # symbol
    this.uppercase = false; // whether to show the color code in upper case
    this.onFineChange = null; // called instantly every time the color changes (value can be either a function or a string with javascript code)
    this.activeClass = 'jscolor-active'; // class to be set to the target element when a picker window is open on it
    this.overwriteImportant = false; // whether to overwrite colors of styleElement using !important
    this.minS = 0; // min allowed saturation (0 - 100)
    this.maxS = 100; // max allowed saturation (0 - 100)
    this.minV = 0; // min allowed value (brightness) (0 - 100)
    this.maxV = 100; // max allowed value (brightness) (0 - 100)

    // Accessing the picked color
    //
    this.hsv = [0, 0, 100]; // read-only  [0-360, 0-100, 0-100]
    this.rgb = [255, 255, 255]; // read-only  [0-255, 0-255, 0-255]

    // Color Picker options
    //
    this.width = 120; // width of color palette (in px)
    this.height = 90; // height of color palette (in px)
    this.showOnClick = true; // whether to display the color picker when user clicks on its target element
    this.mode = 'HSV'; // HSV | HVS | HS | HV - layout of the color picker controls
    this.position = 'bottom'; // left | right | top | bottom - position relative to the target element
    this.smartPosition = true; // automatically change picker position when there is not enough space for it
    this.sliderSize = 20; // px
    this.crossSize = 10; // px
    this.closable = false; // whether to display the Close button
    this.closeText = 'Close';
    this.buttonColor = '#333333'; // CSS color
    this.buttonHeight = 20; // px
    this.padding = 15; // px
    this.backgroundColor = '#ffffff'; // CSS color
    this.borderWidth = 0; // px
    this.borderColor = '#eeeeee'; // CSS color
    this.borderRadius = 0.5; // em
    this.insetWidth = 1; // px
    this.insetColor = '#eeeeee'; // CSS color
    this.shadow = true; // whether to display shadow
    this.shadowBlur = 15; // px
    this.shadowColor = 'rgba(0,0,0,0.1)'; // CSS color
    this.pointerColor = '#333333'; // px
    this.pointerBorderColor = '#ffffff'; // px
        this.pointerBorderWidth = 1; // px
        this.pointerThickness = 2; // px
    this.zIndex = 1000;
    this.container = null; // where to append the color picker (BODY element by default)


    for (var opt in options) {
      if (options.hasOwnProperty(opt)) {
        this[opt] = options[opt];
      }
    }


    this.hide = function () {
      if (isPickerOwner()) {
        detachPicker();
      }
    };


    this.show = function () {
      drawPicker();
    };


    this.redraw = function () {
      if (isPickerOwner()) {
        drawPicker();
      }
    };


    this.importColor = function () {
      if (!this.valueElement) {
        this.exportColor();
      } else {
        if (jsc.isElementType(this.valueElement, 'input')) {
          if (!this.refine) {
            if (!this.fromString(this.valueElement.value, jsc.leaveValue)) {
              if (this.styleElement) {
                this.styleElement.style.backgroundImage = this.styleElement._jscOrigStyle.backgroundImage;
                this.styleElement.style.backgroundColor = this.styleElement._jscOrigStyle.backgroundColor;
                this.styleElement.style.color = this.styleElement._jscOrigStyle.color;
              }
              this.exportColor(jsc.leaveValue | jsc.leaveStyle);
            }
          } else if (!this.required && /^\s*$/.test(this.valueElement.value)) {
            this.valueElement.value = '';
            if (this.styleElement) {
              this.styleElement.style.backgroundImage = this.styleElement._jscOrigStyle.backgroundImage;
              this.styleElement.style.backgroundColor = this.styleElement._jscOrigStyle.backgroundColor;
              this.styleElement.style.color = this.styleElement._jscOrigStyle.color;
            }
            this.exportColor(jsc.leaveValue | jsc.leaveStyle);

          } else if (this.fromString(this.valueElement.value)) {
            // managed to import color successfully from the value -> OK, don't do anything
          } else {
            this.exportColor();
          }
        } else {
          // not an input element -> doesn't have any value
          this.exportColor();
        }
      }
    };


    this.exportColor = function (flags) {
      if (!(flags & jsc.leaveValue) && this.valueElement) {
        var value = this.toString();
        if (this.uppercase) { value = value.toUpperCase(); }
        if (this.hash) { value = '#' + value; }

        if (jsc.isElementType(this.valueElement, 'input')) {
          this.valueElement.value = value;
        } else {
          this.valueElement.innerHTML = value;
        }
      }
      if (!(flags & jsc.leaveStyle)) {
        if (this.styleElement) {
          var bgColor = '#' + this.toString();
          var fgColor = this.isLight() ? '#333' : '#FFF';

          this.styleElement.style.backgroundImage = 'none';
          this.styleElement.style.backgroundColor = bgColor;
          this.styleElement.style.color = fgColor;

          if (this.overwriteImportant) {
            this.styleElement.setAttribute('style',
              'background: ' + bgColor + ' !important; ' +
              'color: ' + fgColor + ' !important;'
            );
          }
        }
      }
      if (!(flags & jsc.leavePad) && isPickerOwner()) {
        redrawPad();
      }
      if (!(flags & jsc.leaveSld) && isPickerOwner()) {
        redrawSld();
      }
    };


    // h: 0-360
    // s: 0-100
    // v: 0-100
    //
    this.fromHSV = function (h, s, v, flags) { // null = don't change
      if (h !== null) {
        if (isNaN(h)) { return false; }
        h = Math.max(0, Math.min(360, h));
      }
      if (s !== null) {
        if (isNaN(s)) { return false; }
        s = Math.max(0, Math.min(100, this.maxS, s), this.minS);
      }
      if (v !== null) {
        if (isNaN(v)) { return false; }
        v = Math.max(0, Math.min(100, this.maxV, v), this.minV);
      }

      this.rgb = HSV_RGB(
        h===null ? this.hsv[0] : (this.hsv[0]=h),
        s===null ? this.hsv[1] : (this.hsv[1]=s),
        v===null ? this.hsv[2] : (this.hsv[2]=v)
      );

      this.exportColor(flags);
    };


    // r: 0-255
    // g: 0-255
    // b: 0-255
    //
    this.fromRGB = function (r, g, b, flags) { // null = don't change
      if (r !== null) {
        if (isNaN(r)) { return false; }
        r = Math.max(0, Math.min(255, r));
      }
      if (g !== null) {
        if (isNaN(g)) { return false; }
        g = Math.max(0, Math.min(255, g));
      }
      if (b !== null) {
        if (isNaN(b)) { return false; }
        b = Math.max(0, Math.min(255, b));
      }

      var hsv = RGB_HSV(
        r===null ? this.rgb[0] : r,
        g===null ? this.rgb[1] : g,
        b===null ? this.rgb[2] : b
      );
      if (hsv[0] !== null) {
        this.hsv[0] = Math.max(0, Math.min(360, hsv[0]));
      }
      if (hsv[2] !== 0) {
        this.hsv[1] = hsv[1]===null ? null : Math.max(0, this.minS, Math.min(100, this.maxS, hsv[1]));
      }
      this.hsv[2] = hsv[2]===null ? null : Math.max(0, this.minV, Math.min(100, this.maxV, hsv[2]));

      // update RGB according to final HSV, as some values might be trimmed
      var rgb = HSV_RGB(this.hsv[0], this.hsv[1], this.hsv[2]);
      this.rgb[0] = rgb[0];
      this.rgb[1] = rgb[1];
      this.rgb[2] = rgb[2];

      this.exportColor(flags);
    };


    this.fromString = function (str, flags) {
      var m;
      if (m = str.match(/^\W*([0-9A-F]{3}([0-9A-F]{3})?)\W*$/i)) {
        // HEX notation
        //

        if (m[1].length === 6 || m[1].length === 8) {
          // 6-char notation
          this.fromRGB(
            parseInt(m[1].substr(0,2),16),
            parseInt(m[1].substr(2,2),16),
            parseInt(m[1].substr(4,2),16),
            flags
          );
        } else {
          // 3-char notation
          this.fromRGB(
            parseInt(m[1].charAt(0) + m[1].charAt(0),16),
            parseInt(m[1].charAt(1) + m[1].charAt(1),16),
            parseInt(m[1].charAt(2) + m[1].charAt(2),16),
            flags
          );
        }
        return true;

      } else if (m = str.match(/^\W*rgba?\(([^)]*)\)\W*$/i)) {
        var params = m[1].split(',');
        var re = /^\s*(\d*)(\.\d+)?\s*$/;
        var mR, mG, mB;
        if (
          params.length >= 3 &&
          (mR = params[0].match(re)) &&
          (mG = params[1].match(re)) &&
          (mB = params[2].match(re))
        ) {
          var r = parseFloat((mR[1] || '0') + (mR[2] || ''));
          var g = parseFloat((mG[1] || '0') + (mG[2] || ''));
          var b = parseFloat((mB[1] || '0') + (mB[2] || ''));
          this.fromRGB(r, g, b, flags);
          return true;
        }
      }
      return false;
    };


    this.toString = function () {
      return (
        (0x100 | Math.round(this.rgb[0])).toString(16).substr(1) +
        (0x100 | Math.round(this.rgb[1])).toString(16).substr(1) +
        (0x100 | Math.round(this.rgb[2])).toString(16).substr(1)
      );
    };


    this.toHEXString = function () {
      return '#' + this.toString().toUpperCase();
    };


    this.toRGBString = function () {
      return ('rgb(' +
        Math.round(this.rgb[0]) + ',' +
        Math.round(this.rgb[1]) + ',' +
        Math.round(this.rgb[2]) + ')'
      );
    };


    this.isLight = function () {
      return (
        0.213 * this.rgb[0] +
        0.715 * this.rgb[1] +
        0.072 * this.rgb[2] >
        255 / 2
      );
    };


    this._processParentElementsInDOM = function () {
      if (this._linkedElementsProcessed) { return; }
      this._linkedElementsProcessed = true;

      var elm = this.targetElement;
      do {
        // If the target element or one of its parent nodes has fixed position,
        // then use fixed positioning instead
        //
        // Note: In Firefox, getComputedStyle returns null in a hidden iframe,
        // that's why we need to check if the returned style object is non-empty
        var currStyle = jsc.getStyle(elm);
        if (currStyle && currStyle.position.toLowerCase() === 'fixed') {
          this.fixed = true;
        }

        if (elm !== this.targetElement) {
          // Ensure to attach onParentScroll only once to each parent element
          // (multiple targetElements can share the same parent nodes)
          //
          // Note: It's not just offsetParents that can be scrollable,
          // that's why we loop through all parent nodes
          if (!elm._jscEventsAttached) {
            jsc.attachEvent(elm, 'scroll', jsc.onParentScroll);
            elm._jscEventsAttached = true;
          }
        }
      } while ((elm = elm.parentNode) && !jsc.isElementType(elm, 'body'));
    };


    // r: 0-255
    // g: 0-255
    // b: 0-255
    //
    // returns: [ 0-360, 0-100, 0-100 ]
    //
    function RGB_HSV (r, g, b) {
      r /= 255;
      g /= 255;
      b /= 255;
      var n = Math.min(Math.min(r,g),b);
      var v = Math.max(Math.max(r,g),b);
      var m = v - n;
      if (m === 0) { return [ null, 0, 100 * v ]; }
      var h = r===n ? 3+(b-g)/m : (g===n ? 5+(r-b)/m : 1+(g-r)/m);
      return [
        60 * (h===6?0:h),
        100 * (m/v),
        100 * v
      ];
    }


    // h: 0-360
    // s: 0-100
    // v: 0-100
    //
    // returns: [ 0-255, 0-255, 0-255 ]
    //
    function HSV_RGB (h, s, v) {
      var u = 255 * (v / 100);

      if (h === null) {
        return [ u, u, u ];
      }

      h /= 60;
      s /= 100;

      var i = Math.floor(h);
      var f = i%2 ? h-i : 1-(h-i);
      var m = u * (1 - s);
      var n = u * (1 - s * f);
      switch (i) {
        case 6:
        case 0: return [u,n,m];
        case 1: return [n,u,m];
        case 2: return [m,u,n];
        case 3: return [m,n,u];
        case 4: return [n,m,u];
        case 5: return [u,m,n];
      }
    }


    function detachPicker () {
      jsc.unsetClass(THIS.targetElement, THIS.activeClass);
      jsc.picker.wrap.parentNode.removeChild(jsc.picker.wrap);
      delete jsc.picker.owner;
    }


    function drawPicker () {

      // At this point, when drawing the picker, we know what the parent elements are
      // and we can do all related DOM operations, such as registering events on them
      // or checking their positioning
      THIS._processParentElementsInDOM();

      if (!jsc.picker) {
        jsc.picker = {
          owner: null,
          wrap : document.createElement('div'),
          box : document.createElement('div'),
          boxS : document.createElement('div'), // shadow area
          boxB : document.createElement('div'), // border
          pad : document.createElement('div'),
          padB : document.createElement('div'), // border
          padM : document.createElement('div'), // mouse/touch area
          padPal : jsc.createPalette(),
          cross : document.createElement('div'),
          crossBY : document.createElement('div'), // border Y
          crossBX : document.createElement('div'), // border X
          crossLY : document.createElement('div'), // line Y
          crossLX : document.createElement('div'), // line X
          sld : document.createElement('div'),
          sldB : document.createElement('div'), // border
          sldM : document.createElement('div'), // mouse/touch area
          sldGrad : jsc.createSliderGradient(),
          sldPtrS : document.createElement('div'), // slider pointer spacer
          sldPtrIB : document.createElement('div'), // slider pointer inner border
          sldPtrMB : document.createElement('div'), // slider pointer middle border
          sldPtrOB : document.createElement('div'), // slider pointer outer border
          btn : document.createElement('div'),
          btnT : document.createElement('span') // text
        };

        jsc.picker.pad.appendChild(jsc.picker.padPal.elm);
        jsc.picker.padB.appendChild(jsc.picker.pad);
        jsc.picker.cross.appendChild(jsc.picker.crossBY);
        jsc.picker.cross.appendChild(jsc.picker.crossBX);
        jsc.picker.cross.appendChild(jsc.picker.crossLY);
        jsc.picker.cross.appendChild(jsc.picker.crossLX);
        jsc.picker.padB.appendChild(jsc.picker.cross);
        jsc.picker.box.appendChild(jsc.picker.padB);
        jsc.picker.box.appendChild(jsc.picker.padM);

        jsc.picker.sld.appendChild(jsc.picker.sldGrad.elm);
        jsc.picker.sldB.appendChild(jsc.picker.sld);
        jsc.picker.sldB.appendChild(jsc.picker.sldPtrOB);
        jsc.picker.sldPtrOB.appendChild(jsc.picker.sldPtrMB);
        jsc.picker.sldPtrMB.appendChild(jsc.picker.sldPtrIB);
        jsc.picker.sldPtrIB.appendChild(jsc.picker.sldPtrS);
        jsc.picker.box.appendChild(jsc.picker.sldB);
        jsc.picker.box.appendChild(jsc.picker.sldM);

        jsc.picker.btn.appendChild(jsc.picker.btnT);
        jsc.picker.box.appendChild(jsc.picker.btn);

        jsc.picker.boxB.appendChild(jsc.picker.box);
        jsc.picker.wrap.appendChild(jsc.picker.boxS);
        jsc.picker.wrap.appendChild(jsc.picker.boxB);
      }

      var p = jsc.picker;

      var displaySlider = !!jsc.getSliderComponent(THIS);
      var dims = jsc.getPickerDims(THIS);
      var crossOuterSize = (2 * THIS.pointerBorderWidth + THIS.pointerThickness + 2 * THIS.crossSize);
      var padToSliderPadding = jsc.getPadToSliderPadding(THIS);
      var borderRadius = Math.min(
        THIS.borderRadius,
        Math.round(THIS.padding * Math.PI)); // px
      var padCursor = 'crosshair';

      // wrap
      p.wrap.style.clear = 'both';
      p.wrap.style.width = (dims[0] + 2 * THIS.borderWidth) + 'px';
      p.wrap.style.height = (dims[1] + 2 * THIS.borderWidth) + 'px';
      p.wrap.style.zIndex = THIS.zIndex;

      // picker
      p.box.style.width = dims[0] + 'px';
      p.box.style.height = dims[1] + 'px';

      p.boxS.style.position = 'absolute';
      p.boxS.style.left = '0';
      p.boxS.style.top = '0';
      p.boxS.style.width = '100%';
      p.boxS.style.height = '100%';
      jsc.setBorderRadius(p.boxS, borderRadius + 'em');

      // picker border
      p.boxB.style.position = 'relative';
      p.boxB.style.border = THIS.borderWidth + 'px solid';
      p.boxB.style.borderColor = THIS.borderColor;
      p.boxB.style.background = THIS.backgroundColor;
      jsc.setBorderRadius(p.boxB, borderRadius + 'em');

      // IE hack:
      // If the element is transparent, IE will trigger the event on the elements under it,
      // e.g. on Canvas or on elements with border
      p.padM.style.background =
      p.sldM.style.background =
        '#FFF';
      jsc.setStyle(p.padM, 'opacity', '0');
      jsc.setStyle(p.sldM, 'opacity', '0');

      // pad
      p.pad.style.position = 'relative';
      p.pad.style.width = THIS.width + 'px';
      p.pad.style.height = THIS.height + 'px';

      // pad palettes (HSV and HVS)
      p.padPal.draw(THIS.width, THIS.height, jsc.getPadYComponent(THIS));

      // pad border
      p.padB.style.position = 'absolute';
      p.padB.style.left = THIS.padding + 'px';
      p.padB.style.top = THIS.padding + 'px';
      p.padB.style.border = THIS.insetWidth + 'px solid';
      p.padB.style.borderColor = THIS.insetColor;

      // pad mouse area
      p.padM._jscInstance = THIS;
      p.padM._jscControlName = 'pad';
      p.padM.style.position = 'absolute';
      p.padM.style.left = '0';
      p.padM.style.top = '0';
      p.padM.style.width = (THIS.padding + 2 * THIS.insetWidth + THIS.width + padToSliderPadding / 2) + 'px';
      p.padM.style.height = dims[1] + 'px';
      p.padM.style.cursor = padCursor;

      // pad cross
      p.cross.style.position = 'absolute';
      p.cross.style.left =
      p.cross.style.top =
        '0';
      p.cross.style.width =
      p.cross.style.height =
        crossOuterSize + 'px';

      // pad cross border Y and X
      p.crossBY.style.position =
      p.crossBX.style.position =
        'absolute';
      p.crossBY.style.background =
      p.crossBX.style.background =
        THIS.pointerBorderColor;
      p.crossBY.style.width =
      p.crossBX.style.height =
        (2 * THIS.pointerBorderWidth + THIS.pointerThickness) + 'px';
      p.crossBY.style.height =
      p.crossBX.style.width =
        crossOuterSize + 'px';
      p.crossBY.style.left =
      p.crossBX.style.top =
        (Math.floor(crossOuterSize / 2) - Math.floor(THIS.pointerThickness / 2) - THIS.pointerBorderWidth) + 'px';
      p.crossBY.style.top =
      p.crossBX.style.left =
        '0';

      // pad cross line Y and X
      p.crossLY.style.position =
      p.crossLX.style.position =
        'absolute';
      p.crossLY.style.background =
      p.crossLX.style.background =
        THIS.pointerColor;
      p.crossLY.style.height =
      p.crossLX.style.width =
        (crossOuterSize - 2 * THIS.pointerBorderWidth) + 'px';
      p.crossLY.style.width =
      p.crossLX.style.height =
        THIS.pointerThickness + 'px';
      p.crossLY.style.left =
      p.crossLX.style.top =
        (Math.floor(crossOuterSize / 2) - Math.floor(THIS.pointerThickness / 2)) + 'px';
      p.crossLY.style.top =
      p.crossLX.style.left =
        THIS.pointerBorderWidth + 'px';

      // slider
      p.sld.style.overflow = 'hidden';
      p.sld.style.width = THIS.sliderSize + 'px';
      p.sld.style.height = THIS.height + 'px';

      // slider gradient
      p.sldGrad.draw(THIS.sliderSize, THIS.height, '#000', '#000');

      // slider border
      p.sldB.style.display = displaySlider ? 'block' : 'none';
      p.sldB.style.position = 'absolute';
      p.sldB.style.right = THIS.padding + 'px';
      p.sldB.style.top = THIS.padding + 'px';
      p.sldB.style.border = THIS.insetWidth + 'px solid';
      p.sldB.style.borderColor = THIS.insetColor;

      // slider mouse area
      p.sldM._jscInstance = THIS;
      p.sldM._jscControlName = 'sld';
      p.sldM.style.display = displaySlider ? 'block' : 'none';
      p.sldM.style.position = 'absolute';
      p.sldM.style.right = '0';
      p.sldM.style.top = '0';
      p.sldM.style.width = (THIS.sliderSize + padToSliderPadding / 2 + THIS.padding + 2 * THIS.insetWidth) + 'px';
      p.sldM.style.height = dims[1] + 'px';
      p.sldM.style.cursor = 'default';

      // slider pointer inner and outer border
      p.sldPtrIB.style.border =
      p.sldPtrOB.style.border =
        THIS.pointerBorderWidth + 'px solid ' + THIS.pointerBorderColor;

      // slider pointer outer border
      p.sldPtrOB.style.position = 'absolute';
      p.sldPtrOB.style.left = -(2 * THIS.pointerBorderWidth + THIS.pointerThickness) + 'px';
      p.sldPtrOB.style.top = '0';

      // slider pointer middle border
      p.sldPtrMB.style.border = THIS.pointerThickness + 'px solid ' + THIS.pointerColor;

      // slider pointer spacer
      p.sldPtrS.style.width = THIS.sliderSize + 'px';
      p.sldPtrS.style.height = sliderPtrSpace + 'px';

      // the Close button
      function setBtnBorder () {
        var insetColors = THIS.insetColor.split(/\s+/);
        var outsetColor = insetColors.length < 2 ? insetColors[0] : insetColors[1] + ' ' + insetColors[0] + ' ' + insetColors[0] + ' ' + insetColors[1];
        p.btn.style.borderColor = outsetColor;
      }
      p.btn.style.display = THIS.closable ? 'block' : 'none';
      p.btn.style.position = 'absolute';
      p.btn.style.left = THIS.padding + 'px';
      p.btn.style.bottom = THIS.padding + 'px';
      p.btn.style.padding = '0 15px';
      p.btn.style.height = THIS.buttonHeight + 'px';
      p.btn.style.border = THIS.insetWidth + 'px solid';
      setBtnBorder();
      p.btn.style.color = THIS.buttonColor;
      p.btn.style.font = '12px sans-serif';
      p.btn.style.textAlign = 'center';
      try {
        p.btn.style.cursor = 'pointer';
      } catch(eOldIE) {
        p.btn.style.cursor = 'hand';
      }
      p.btn.onmousedown = function () {
        THIS.hide();
      };
      p.btnT.style.lineHeight = THIS.buttonHeight + 'px';
      p.btnT.innerHTML = '';
      p.btnT.appendChild(document.createTextNode(THIS.closeText));

      // place pointers
      redrawPad();
      redrawSld();

      // If we are changing the owner without first closing the picker,
      // make sure to first deal with the old owner
      if (jsc.picker.owner && jsc.picker.owner !== THIS) {
        jsc.unsetClass(jsc.picker.owner.targetElement, THIS.activeClass);
      }

      // Set the new picker owner
      jsc.picker.owner = THIS;

      // The redrawPosition() method needs picker.owner to be set, that's why we call it here,
      // after setting the owner
      if (jsc.isElementType(container, 'body')) {
        jsc.redrawPosition();
      } else {
        jsc._drawPosition(THIS, 0, 0, 'relative', false);
      }

      if (p.wrap.parentNode != container) {
        container.appendChild(p.wrap);
      }

      jsc.setClass(THIS.targetElement, THIS.activeClass);
    }


    function redrawPad () {
      // redraw the pad pointer
      switch (jsc.getPadYComponent(THIS)) {
      case 's': var yComponent = 1; break;
      case 'v': var yComponent = 2; break;
      }
      var x = Math.round((THIS.hsv[0] / 360) * (THIS.width - 1));
      var y = Math.round((1 - THIS.hsv[yComponent] / 100) * (THIS.height - 1));
      var crossOuterSize = (2 * THIS.pointerBorderWidth + THIS.pointerThickness + 2 * THIS.crossSize);
      var ofs = -Math.floor(crossOuterSize / 2);
      jsc.picker.cross.style.left = (x + ofs) + 'px';
      jsc.picker.cross.style.top = (y + ofs) + 'px';

      // redraw the slider
      switch (jsc.getSliderComponent(THIS)) {
      case 's':
        var rgb1 = HSV_RGB(THIS.hsv[0], 100, THIS.hsv[2]);
        var rgb2 = HSV_RGB(THIS.hsv[0], 0, THIS.hsv[2]);
        var color1 = 'rgb(' +
          Math.round(rgb1[0]) + ',' +
          Math.round(rgb1[1]) + ',' +
          Math.round(rgb1[2]) + ')';
        var color2 = 'rgb(' +
          Math.round(rgb2[0]) + ',' +
          Math.round(rgb2[1]) + ',' +
          Math.round(rgb2[2]) + ')';
        jsc.picker.sldGrad.draw(THIS.sliderSize, THIS.height, color1, color2);
        break;
      case 'v':
        var rgb = HSV_RGB(THIS.hsv[0], THIS.hsv[1], 100);
        var color1 = 'rgb(' +
          Math.round(rgb[0]) + ',' +
          Math.round(rgb[1]) + ',' +
          Math.round(rgb[2]) + ')';
        var color2 = '#000';
        jsc.picker.sldGrad.draw(THIS.sliderSize, THIS.height, color1, color2);
        break;
      }
    }


    function redrawSld () {
      var sldComponent = jsc.getSliderComponent(THIS);
      if (sldComponent) {
        // redraw the slider pointer
        switch (sldComponent) {
        case 's': var yComponent = 1; break;
        case 'v': var yComponent = 2; break;
        }
        var y = Math.round((1 - THIS.hsv[yComponent] / 100) * (THIS.height - 1));
        jsc.picker.sldPtrOB.style.top = (y - (2 * THIS.pointerBorderWidth + THIS.pointerThickness) - Math.floor(sliderPtrSpace / 2)) + 'px';
      }
    }


    function isPickerOwner () {
      return jsc.picker && jsc.picker.owner === THIS;
    }


    function blurValue () {
      THIS.importColor();
    }


    // Find the target element
    if (typeof targetElement === 'string') {
      var id = targetElement;
      var elm = document.getElementById(id);
      if (elm) {
        this.targetElement = elm;
      } else {
        jsc.warn('Could not find target element with ID \'' + id + '\'');
      }
    } else if (targetElement) {
      this.targetElement = targetElement;
    } else {
      jsc.warn('Invalid target element: \'' + targetElement + '\'');
    }

    if (this.targetElement._jscLinkedInstance) {
      jsc.warn('Cannot link jscolor twice to the same element. Skipping.');
      return;
    }
    this.targetElement._jscLinkedInstance = this;

    // Find the value element
    this.valueElement = jsc.fetchElement(this.valueElement);
    // Find the style element
    this.styleElement = jsc.fetchElement(this.styleElement);

    var THIS = this;
    var container =
      this.container ?
      jsc.fetchElement(this.container) :
      document.getElementsByTagName('body')[0];
    var sliderPtrSpace = 3; // px

    // For BUTTON elements it's important to stop them from sending the form when clicked
    // (e.g. in Safari)
    if (jsc.isElementType(this.targetElement, 'button')) {
      if (this.targetElement.onclick) {
        var origCallback = this.targetElement.onclick;
        this.targetElement.onclick = function (evt) {
          origCallback.call(this, evt);
          return false;
        };
      } else {
        this.targetElement.onclick = function () { return false; };
      }
    }

    /*
    var elm = this.targetElement;
    do {
      // If the target element or one of its offsetParents has fixed position,
      // then use fixed positioning instead
      //
      // Note: In Firefox, getComputedStyle returns null in a hidden iframe,
      // that's why we need to check if the returned style object is non-empty
      var currStyle = jsc.getStyle(elm);
      if (currStyle && currStyle.position.toLowerCase() === 'fixed') {
        this.fixed = true;
      }

      if (elm !== this.targetElement) {
        // attach onParentScroll so that we can recompute the picker position
        // when one of the offsetParents is scrolled
        if (!elm._jscEventsAttached) {
          jsc.attachEvent(elm, 'scroll', jsc.onParentScroll);
          elm._jscEventsAttached = true;
        }
      }
    } while ((elm = elm.offsetParent) && !jsc.isElementType(elm, 'body'));
    */

    // valueElement
    if (this.valueElement) {
      if (jsc.isElementType(this.valueElement, 'input')) {
        var updateField = function () {
          THIS.fromString(THIS.valueElement.value, jsc.leaveValue);
          jsc.dispatchFineChange(THIS);
        };
        jsc.attachEvent(this.valueElement, 'keyup', updateField);
        jsc.attachEvent(this.valueElement, 'input', updateField);
        jsc.attachEvent(this.valueElement, 'blur', blurValue);
        this.valueElement.setAttribute('autocomplete', 'off');
      }
    }

    // styleElement
    if (this.styleElement) {
      this.styleElement._jscOrigStyle = {
        backgroundImage : this.styleElement.style.backgroundImage,
        backgroundColor : this.styleElement.style.backgroundColor,
        color : this.styleElement.style.color
      };
    }

    if (this.value) {
      // Try to set the color from the .value option and if unsuccessful,
      // export the current color
      this.fromString(this.value) || this.exportColor();
    } else {
      this.importColor();
    }
  }

};


//================================
// Public properties and methods
//================================


// By default, search for all elements with class="jscolor" and install a color picker on them.
//
// You can change what class name will be looked for by setting the property jscolor.lookupClass
// anywhere in your HTML document. To completely disable the automatic lookup, set it to null.
//
jsc.jscolor.lookupClass = 'jscolor';


jsc.jscolor.installByClassName = function (className) {
  var inputElms = document.getElementsByTagName('input');
  var buttonElms = document.getElementsByTagName('button');

  jsc.tryInstallOnElements(inputElms, className);
  jsc.tryInstallOnElements(buttonElms, className);
};


jsc.register();


return jsc.jscolor;


})(); }

const OnMouseEvent = EventHole('mousedown', 'mouseup', 'mouseout', 'mousemove');
const OnTouchEvent = EventHole('touchstart', 'touchmove', 'touchend');

window.addEventListener('load', function(e) {
  Select.init('.modal select');
  Board.resize();
});

