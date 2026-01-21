/**
 * HeroForge Character STL Exporter
 *
 * Exports the currently displayed HeroForge 3D character as STL files.
 * Separates base/pedestal meshes from character meshes and exports as a ZIP.
 *
 * Usage:
 *   1. Import and call exportCharacter() from the browser console
 *   2. Or paste the compiled JS version directly
 */

// ============================================================
// Type Declarations for HeroForge/RenderKit globals
// ============================================================

declare const CK: {
  activeDisplay: {
    meshes: Record<string, Mesh>
  }
  scene: {
    updateMatrixWorld(force: boolean): void
  }
}

declare const RK: {
  Matrix4: new () => Matrix4
  Vec3: new (x: number, y: number, z: number) => Vec3
}

// ============================================================
// Type Definitions
// ============================================================

interface Vec3 {
  x: number
  y: number
  z: number
  clone(): Vec3
  copy(v: Vec3): Vec3
  applyMatrix4(m: Matrix4): Vec3
  addScaledVector(v: Vec3, s: number): Vec3
  divideScalar(s: number): Vec3
}

interface Matrix4 {
  elements: number[]
  copy(m: Matrix4): Matrix4
  multiply(m: Matrix4): Matrix4
}

interface BufferAttribute {
  count: number
  array: ArrayLike<number>
  getX(index: number): number
  getY(index: number): number
  getZ(index: number): number
  getW(index: number): number
}

interface BufferGeometry {
  type: string
  attributes: {
    position: BufferAttribute
    skin0?: BufferAttribute
    skin1?: BufferAttribute
    skin2?: BufferAttribute
    skin3?: BufferAttribute
    [key: string]: BufferAttribute | undefined
  }
  index: { array: ArrayLike<number> } | null
  morphAttributes?: {
    position?: BufferAttribute[]
  }
}

interface Bone {
  matrixWorld: Matrix4
  getMatrixWorld?(): Matrix4
  updateMatrixWorld(force: boolean): void
}

interface Skeleton {
  bones: Bone[]
  boneInverses: Matrix4[]
  update?(): void
}
interface TextureLike {
  name?: string
  image?: HTMLImageElement | HTMLCanvasElement | ImageBitmap | OffscreenCanvas | { width: number; height: number } | null
  source?: { data?: HTMLImageElement | HTMLCanvasElement | ImageBitmap | null } // some engines wrap it
  userData?: any
  uuid?: string
}

interface MaterialLike {
  name?: string
  map?: TextureLike | null
  normalMap?: TextureLike | null
  roughnessMap?: TextureLike | null
  metalnessMap?: TextureLike | null
  aoMap?: TextureLike | null
  emissiveMap?: TextureLike | null
  alphaMap?: TextureLike | null
  // Some renderers store colors/params too, but optional
  [key: string]: any
}

interface Mesh {
  isMesh: boolean
  isSkinnedMesh: boolean
  visible: boolean
  name: string
  geometry: BufferGeometry
  skeleton?: Skeleton
  matrixWorld: Matrix4
  bindMatrix?: Matrix4
  bindMatrixInverse?: Matrix4
  morphTargetInfluences?: number[]
  children?: Mesh[]
  material?: MaterialLike | MaterialLike[]
  updateMatrixWorld(force: boolean): void
}

interface Vertex {
  x: number
  y: number
  z: number
}

interface Triangle {
  v1: Vertex
  v2: Vertex
  v3: Vertex
}

interface ExportOptions {
  filename?: string
  scale?: number
  separateBase?: boolean
  format?: 'stl' | 'obj'
  includeTextures?: boolean
  includeNormals?: boolean
}

interface CollectedMeshes {
  characterMeshes: Mesh[]
  baseMeshes: Mesh[]
}

interface ZipFile {
  name: string
  data: Uint8Array
}

interface LocalHeader {
  header: ArrayBuffer
  data: Uint8Array
  offset: number
}

// ============================================================
// SimpleZip - Minimal ZIP library
// ============================================================

/**
 * Simple ZIP file creator
 * Creates uncompressed ZIP files (STORE method)
 */
class SimpleZip {
  private files: ZipFile[] = []
  private static _crc32Table: Uint32Array | null = null

  addFile(name: string, data: ArrayBuffer): void {
    this.files.push({ name, data: new Uint8Array(data) })
  }

  generate(): ArrayBuffer {
    const localHeaders: LocalHeader[] = []
    const centralHeaders: ArrayBuffer[] = []
    let offset = 0

    // Create local file headers and file data
    for (const file of this.files) {
      const nameBytes = new TextEncoder().encode(file.name)
      const localHeader = this._createLocalHeader(nameBytes, file.data)
      localHeaders.push({ header: localHeader, data: file.data, offset })
      offset += localHeader.byteLength + file.data.byteLength
    }

    // Create central directory
    const centralStart = offset
    for (let i = 0; i < this.files.length; i++) {
      const file = this.files[i]
      const nameBytes = new TextEncoder().encode(file.name)
      const centralHeader = this._createCentralHeader(nameBytes, file.data, localHeaders[i].offset)
      centralHeaders.push(centralHeader)
      offset += centralHeader.byteLength
    }
    const centralEnd = offset

    // Create end of central directory
    const eocd = this._createEOCD(this.files.length, centralEnd - centralStart, centralStart)
    offset += eocd.byteLength

    // Combine all parts
    const result = new Uint8Array(offset)
    let pos = 0

    for (const local of localHeaders) {
      result.set(new Uint8Array(local.header), pos)
      pos += local.header.byteLength
      result.set(local.data, pos)
      pos += local.data.byteLength
    }

    for (const central of centralHeaders) {
      result.set(new Uint8Array(central), pos)
      pos += central.byteLength
    }

    result.set(new Uint8Array(eocd), pos)

    return result.buffer
  }

  private _createLocalHeader(nameBytes: Uint8Array, data: Uint8Array): ArrayBuffer {
    const header = new ArrayBuffer(30 + nameBytes.length)
    const view = new DataView(header)

    view.setUint32(0, 0x04034b50, true) // Local file header signature
    view.setUint16(4, 20, true) // Version needed
    view.setUint16(6, 0, true) // General purpose bit flag
    view.setUint16(8, 0, true) // Compression method (STORE)
    view.setUint16(10, 0, true) // File last mod time
    view.setUint16(12, 0, true) // File last mod date
    view.setUint32(14, this._crc32(data), true) // CRC-32
    view.setUint32(18, data.byteLength, true) // Compressed size
    view.setUint32(22, data.byteLength, true) // Uncompressed size
    view.setUint16(26, nameBytes.length, true) // File name length
    view.setUint16(28, 0, true) // Extra field length

    new Uint8Array(header).set(nameBytes, 30)

    return header
  }

  private _createCentralHeader(nameBytes: Uint8Array, data: Uint8Array, localOffset: number): ArrayBuffer {
    const header = new ArrayBuffer(46 + nameBytes.length)
    const view = new DataView(header)

    view.setUint32(0, 0x02014b50, true) // Central directory signature
    view.setUint16(4, 20, true) // Version made by
    view.setUint16(6, 20, true) // Version needed
    view.setUint16(8, 0, true) // General purpose bit flag
    view.setUint16(10, 0, true) // Compression method (STORE)
    view.setUint16(12, 0, true) // File last mod time
    view.setUint16(14, 0, true) // File last mod date
    view.setUint32(16, this._crc32(data), true) // CRC-32
    view.setUint32(20, data.byteLength, true) // Compressed size
    view.setUint32(24, data.byteLength, true) // Uncompressed size
    view.setUint16(28, nameBytes.length, true) // File name length
    view.setUint16(30, 0, true) // Extra field length
    view.setUint16(32, 0, true) // File comment length
    view.setUint16(34, 0, true) // Disk number start
    view.setUint16(36, 0, true) // Internal file attributes
    view.setUint32(38, 0, true) // External file attributes
    view.setUint32(42, localOffset, true) // Relative offset of local header

    new Uint8Array(header).set(nameBytes, 46)

    return header
  }

  private _createEOCD(numFiles: number, centralSize: number, centralOffset: number): ArrayBuffer {
    const eocd = new ArrayBuffer(22)
    const view = new DataView(eocd)

    view.setUint32(0, 0x06054b50, true) // EOCD signature
    view.setUint16(4, 0, true) // Disk number
    view.setUint16(6, 0, true) // Disk with central directory
    view.setUint16(8, numFiles, true) // Number of entries on this disk
    view.setUint16(10, numFiles, true) // Total number of entries
    view.setUint32(12, centralSize, true) // Size of central directory
    view.setUint32(16, centralOffset, true) // Offset of central directory
    view.setUint16(20, 0, true) // Comment length

    return eocd
  }

  private _crc32(data: Uint8Array): number {
    let crc = 0xffffffff
    const table = SimpleZip._crc32Table || (SimpleZip._crc32Table = this._makeCRCTable())

    for (let i = 0; i < data.length; i++) {
      crc = (crc >>> 8) ^ table[(crc ^ data[i]) & 0xff]
    }

    return (crc ^ 0xffffffff) >>> 0
  }

  private _makeCRCTable(): Uint32Array {
    const table = new Uint32Array(256)
    for (let i = 0; i < 256; i++) {
      let c = i
      for (let j = 0; j < 8; j++) {
        c = c & 1 ? 0xedb88320 ^ (c >>> 1) : c >>> 1
      }
      table[i] = c
    }
    return table
  }
}

// ============================================================
// Skinning and Mesh Processing Functions
// ============================================================

/**
 * Decode HeroForge skin weight encoding
 * From shader: abs(mod(weight + 1.0, 2.0) - 1.0)
 */
function decodeWeight(encodedWeight: number): number {
  const mod = (n: number, m: number): number => ((n % m) + m) % m
  return Math.abs(mod(encodedWeight + 1, 2) - 1)
}

/**
 * Get bone matrix - computes skinMatrix (bone.matrixWorld * boneInverse)
 */
function getBoneMatrix(skeleton: Skeleton, boneIndex: number): Matrix4 {
  const tempMatrix = new RK.Matrix4()

  const maxBoneIndex = skeleton.bones ? skeleton.bones.length - 1 : -1
  if (boneIndex < 0 || boneIndex > maxBoneIndex) {
    return tempMatrix
  }

  const bone = skeleton.bones[boneIndex]
  const boneInverse = skeleton.boneInverses ? skeleton.boneInverses[boneIndex] : null

  if (bone && boneInverse) {
    const boneWorld = bone.matrixWorld || (bone.getMatrixWorld ? bone.getMatrixWorld() : null)
    if (boneWorld) {
      tempMatrix.copy(boneWorld)
      tempMatrix.multiply(boneInverse)
      return tempMatrix
    }
  }

  return tempMatrix
}

/**
 * Apply skinning to a vertex with a custom position (for morph targets)
 */
function skinVertexWithPosition(mesh: Mesh, vertexIndex: number, x: number, y: number, z: number): Vec3 {
  const geometry = mesh.geometry
  const skeleton = mesh.skeleton
  const position = new RK.Vec3(x, y, z)

  const skinAttrs: BufferAttribute[] = []
  for (let i = 0; i < 4; i++) {
    const attr = geometry.attributes[`skin${i}`]
    if (attr) {
      skinAttrs.push(attr)
    }
  }

  if (skinAttrs.length === 0 || !skeleton) {
    position.applyMatrix4(mesh.matrixWorld)
    return position
  }

  const bindIsIdentity = isIdentityMatrix(mesh.bindMatrix)

  const skinVert = position.clone()
  if (!bindIsIdentity && mesh.bindMatrix) {
    skinVert.applyMatrix4(mesh.bindMatrix)
  }

  const skinned = new RK.Vec3(0, 0, 0)
  let skinSum = 0

  for (const skinAttr of skinAttrs) {
    const boneIdx0 = Math.floor(skinAttr.getX(vertexIndex))
    const weight0 = decodeWeight(skinAttr.getY(vertexIndex))
    const boneIdx1 = Math.floor(skinAttr.getZ(vertexIndex))
    const weight1 = decodeWeight(skinAttr.getW(vertexIndex))

    if (weight0 > 0.0001) {
      const boneMatrix = getBoneMatrix(skeleton, boneIdx0)
      const transformed = skinVert.clone()
      transformed.applyMatrix4(boneMatrix)
      skinned.addScaledVector(transformed, weight0)
      skinSum += weight0
    }

    if (weight1 > 0.0001) {
      const boneMatrix = getBoneMatrix(skeleton, boneIdx1)
      const transformed = skinVert.clone()
      transformed.applyMatrix4(boneMatrix)
      skinned.addScaledVector(transformed, weight1)
      skinSum += weight1
    }
  }

  if (skinSum > 0.0001) {
    skinned.divideScalar(skinSum)
  } else {
    skinned.copy(skinVert)
  }

  if (!bindIsIdentity && mesh.bindMatrixInverse) {
    skinned.applyMatrix4(mesh.bindMatrixInverse)
  }

  return skinned
}

/**
 * Check if a matrix is identity (or close to it)
 */
function isIdentityMatrix(matrix: Matrix4 | undefined): boolean {
  if (!matrix) return true
  const e = matrix.elements
  const epsilon = 0.0001
  return (
    Math.abs(e[0] - 1) < epsilon &&
    Math.abs(e[1]) < epsilon &&
    Math.abs(e[2]) < epsilon &&
    Math.abs(e[3]) < epsilon &&
    Math.abs(e[4]) < epsilon &&
    Math.abs(e[5] - 1) < epsilon &&
    Math.abs(e[6]) < epsilon &&
    Math.abs(e[7]) < epsilon &&
    Math.abs(e[8]) < epsilon &&
    Math.abs(e[9]) < epsilon &&
    Math.abs(e[10] - 1) < epsilon &&
    Math.abs(e[11]) < epsilon &&
    Math.abs(e[12]) < epsilon &&
    Math.abs(e[13]) < epsilon &&
    Math.abs(e[14]) < epsilon &&
    Math.abs(e[15] - 1) < epsilon
  )
}

/**
 * Check if a slot/mesh name indicates it's part of the base/pedestal
 */
function isBaseMesh(slotName: string | undefined, meshName: string | undefined): boolean {
  const lowerSlot = (slotName || '').toLowerCase()
  const lowerName = (meshName || '').toLowerCase()

  // Check if slot or mesh name starts with "base" or contains base-related terms
  return (
    lowerSlot.startsWith('base') ||
    lowerName.startsWith('base') ||
    lowerSlot.includes('pedestal') ||
    lowerName.includes('pedestal')
  )
}

/**
 * Check if a mesh should be included in export
 */
function shouldIncludeMesh(mesh: Mesh, slotName: string | undefined): boolean {
  if (!mesh || !mesh.geometry) return false
  if (!mesh.visible) return false
  if (slotName?.startsWith('_')) return false
  if (mesh.name === 'facePads' || mesh.name === '_debugPass') return false

  const posAttr = mesh.geometry.attributes.position
  if (!posAttr || posAttr.count < 3) return false

  if (mesh.geometry.type === 'PlaneBufferGeometry' || mesh.geometry.type === 'PlaneGeometry') {
    return false
  }

  const lowerName = (mesh.name || '').toLowerCase()
  if (
    lowerName.includes('ground') ||
    lowerName.includes('floor') ||
    lowerName.includes('plane') ||
    lowerName.includes('shadow') ||
    lowerName.includes('reflector')
  ) {
    return false
  }

  const lowerSlot = (slotName || '').toLowerCase()
  if (
    lowerSlot.includes('ground') ||
    lowerSlot.includes('floor') ||
    lowerSlot.includes('plane') ||
    lowerSlot.includes('shadow') ||
    lowerSlot.includes('reflector')
  ) {
    return false
  }

  return true
}

/**
 * Collect all visible meshes, separated into character and base meshes
 */
function collectMeshes(): CollectedMeshes {
  const characterMeshes: Mesh[] = []
  const baseMeshes: Mesh[] = []
  const display = CK.activeDisplay

  if (!display || !display.meshes) {
    console.error('No active display found.')
    return { characterMeshes, baseMeshes }
  }

  function collectRecursive(object: Mesh, slotName: string | undefined): void {
    if (!object) return
    if ((object.isMesh || object.isSkinnedMesh) && shouldIncludeMesh(object, slotName)) {
      if (isBaseMesh(slotName, object.name)) {
        baseMeshes.push(object)
      } else {
        characterMeshes.push(object)
      }
    }
    if (object.children) {
      for (const child of object.children) {
        if (child.name !== 'facePads' && child.name !== '_debugPass') {
          collectRecursive(child, slotName)
        }
      }
    }
  }

  for (const [slotName, mesh] of Object.entries(display.meshes)) {
    if (!mesh || slotName.startsWith('_')) continue
    collectRecursive(mesh, slotName)
  }

  console.log(`Collected ${characterMeshes.length} character meshes, ${baseMeshes.length} base meshes`)
  return { characterMeshes, baseMeshes }
}

interface ExtractedGeometry {
  positions: number[]
  indices: ArrayLike<number> | null
}

/**
 * Extract posed geometry from a mesh
 */
function extractMeshGeometry(mesh: Mesh): ExtractedGeometry {
  const geometry = mesh.geometry
  const positionAttr = geometry.attributes.position
  const count = positionAttr.count

  mesh.updateMatrixWorld(true)
  if (mesh.skeleton) {
    if (mesh.skeleton.bones) {
      for (const bone of mesh.skeleton.bones) {
        if (bone) bone.updateMatrixWorld(true)
      }
    }
    if (mesh.skeleton.update) {
      mesh.skeleton.update()
    }
  }

  const hasMorphTargets = geometry.morphAttributes?.position && geometry.morphAttributes.position.length > 0
  const morphInfluences = mesh.morphTargetInfluences || []

  const positions: number[] = []
  const indices = geometry.index ? geometry.index.array : null

  const isSkinnedMesh = mesh.isSkinnedMesh && mesh.skeleton && geometry.attributes.skin0

  for (let i = 0; i < count; i++) {
    let baseX = positionAttr.getX(i)
    let baseY = positionAttr.getY(i)
    let baseZ = positionAttr.getZ(i)

    if (hasMorphTargets && geometry.morphAttributes?.position) {
      const morphPositions = geometry.morphAttributes.position
      for (let m = 0; m < morphPositions.length; m++) {
        const influence = morphInfluences[m] || 0
        if (influence > 0.0001) {
          const morphAttr = morphPositions[m]
          baseX += morphAttr.getX(i) * influence
          baseY += morphAttr.getY(i) * influence
          baseZ += morphAttr.getZ(i) * influence
        }
      }
    }

    let vertex: Vec3

    if (isSkinnedMesh) {
      vertex = skinVertexWithPosition(mesh, i, baseX, baseY, baseZ)
    } else {
      vertex = new RK.Vec3(baseX, baseY, baseZ)
      vertex.applyMatrix4(mesh.matrixWorld)
    }

    positions.push(vertex.x, vertex.y, vertex.z)
  }

  return { positions, indices }
}
interface ExtractedGeometryExtended extends ExtractedGeometry {
  uvs: number[] | null
}

function extractMeshGeometryExtended(mesh: Mesh): ExtractedGeometryExtended {
  const base = extractMeshGeometry(mesh)

  const uvAttr = mesh.geometry.attributes['uv'] as BufferAttribute | undefined
  let uvs: number[] | null = null
  if (uvAttr && uvAttr.count > 0) {
    // uv is 2 components; BufferAttribute interface only exposes getX/getY so that’s fine
    uvs = new Array(uvAttr.count * 2)
    for (let i = 0; i < uvAttr.count; i++) {
      uvs[i * 2] = uvAttr.getX(i)
      uvs[i * 2 + 1] = uvAttr.getY(i)
    }
  }

  return { ...base, uvs }
}

function pickMaterial(mesh: Mesh): MaterialLike | null {
  const mat = mesh.material
  if (!mat) return null
  return Array.isArray(mat) ? (mat[0] || null) : mat
}

function safeMaterialName(mat: MaterialLike | null, fallback: string): string {
  const raw = (mat?.name || fallback || 'material').trim()
  return raw.replace(/[^\w\-]+/g, '_')
}

function getTextureSlots(mat: MaterialLike | null): Array<{ slot: string; tex: TextureLike }> {
  if (!mat) return []
  const slots = ['map', 'normalMap', 'roughnessMap', 'metalnessMap', 'aoMap', 'emissiveMap', 'alphaMap']
  const out: Array<{ slot: string; tex: TextureLike }> = []
  for (const s of slots) {
    const t = mat[s]
    if (t && typeof t === 'object') out.push({ slot: s, tex: t })
  }
  return out
}
async function textureToPNG(tex: TextureLike): Promise<ArrayBuffer | null> {
  try {
    const img =
      (tex?.image as any) ||
      (tex?.source?.data as any) ||
      null

    if (!img) return null

    // Determine dimensions
    const w = (img.width ?? img.videoWidth ?? img.naturalWidth) || 0
    const h = (img.height ?? img.videoHeight ?? img.naturalHeight) || 0
    if (!w || !h) return null

    const canvas = document.createElement('canvas')
    canvas.width = w
    canvas.height = h
    const ctx = canvas.getContext('2d')
    if (!ctx) return null

    // Draw supported image-like types
    // HTMLImageElement/HTMLCanvasElement/ImageBitmap are handled by drawImage
    ctx.drawImage(img as any, 0, 0, w, h)

    const blob: Blob = await new Promise((resolve, reject) => {
      canvas.toBlob((b) => (b ? resolve(b) : reject(new Error('toBlob failed'))), 'image/png')
    })

    return await blob.arrayBuffer()
  } catch (e) {
    console.warn('Failed to convert texture to PNG:', e)
    return null
  }
}
// Compute a face normal for OBJ normals (same as your STL normal calc)
function faceNormal(a: Vertex, b: Vertex, c: Vertex): Vertex {
  return calculateNormal(a, b, c)
}

function buildOBJFromMeshes(
  meshes: Mesh[],
  filenameBase: string,
  scale: number,
  includeNormals: boolean
): { obj: string; groups: Array<{ materialName: string; faces: string[] }>; v: Vertex[]; vt: { u: number; v: number }[]; vn: Vertex[] } {
  // We’ll build unified OBJ buffers and keep per-mesh material group faces.
  const v: Vertex[] = []
  const vt: { u: number; v: number }[] = []
  const vn: Vertex[] = []

  const groups: Array<{ materialName: string; faces: string[] }> = []

  for (const mesh of meshes) {
    const { positions, indices, uvs } = extractMeshGeometryExtended(mesh)

    // Build vertices (apply same orientation/ground as STL later, but we’ll do per-vertex transform now)
    const verts: Vertex[] = []
    for (let i = 0; i < positions.length; i += 3) {
      const x = positions[i]
      const y = positions[i + 1]
      const z = positions[i + 2]

      // Match your STL transform: (x, y, z) -> (x, -z, y) and scale
      const tx = x * scale
      const ty = -z * scale
      const tz = y * scale
      verts.push({ x: tx, y: ty, z: tz })
    }

    // Determine minZ for ground offset similar to STL.
    // We can do a global ground shift across all meshes after concatenation,
    // but easiest is: push verts first and ground-shift globally after.
    // So: push as-is, then we’ll ground-shift all at the end.
    const meshVStart = v.length
    v.push(...verts)

    // UVs
    let meshVTStart = vt.length
    if (uvs) {
      for (let i = 0; i < uvs.length; i += 2) {
        // OBJ vt v-axis is often flipped depending on convention; many engines use v=1-v.
        // We’ll keep as-is; if upside down, change to (1 - uvs[i+1]).
        vt.push({ u: uvs[i], v: uvs[i + 1] })
      }
    } else {
      meshVTStart = -1
    }

    // Faces grouped by material
    const mat = pickMaterial(mesh)
    const matName = safeMaterialName(mat, mesh.name || 'mesh')

    const faces: string[] = []

    const addFace = (i0: number, i1: number, i2: number): void => {
      // Vertex indices
      const a = meshVStart + i0 + 1
      const b = meshVStart + i1 + 1
      const c = meshVStart + i2 + 1

      // UV indices (if present)
      const ta = uvs ? (meshVTStart + i0 + 1) : 0
      const tb = uvs ? (meshVTStart + i1 + 1) : 0
      const tc = uvs ? (meshVTStart + i2 + 1) : 0

      if (includeNormals) {
        // Compute a per-face normal and store 1 normal per face
        const na = v[meshVStart + i0]
        const nb = v[meshVStart + i1]
        const nc = v[meshVStart + i2]
        const n = faceNormal(na, nb, nc)
        vn.push(n)
        const nIdx = vn.length // 1-based

        if (uvs) {
          faces.push(`f ${a}/${ta}/${nIdx} ${b}/${tb}/${nIdx} ${c}/${tc}/${nIdx}`)
        } else {
          faces.push(`f ${a}//${nIdx} ${b}//${nIdx} ${c}//${nIdx}`)
        }
      } else {
        if (uvs) {
          faces.push(`f ${a}/${ta} ${b}/${tb} ${c}/${tc}`)
        } else {
          faces.push(`f ${a} ${b} ${c}`)
        }
      }
    }

    if (indices) {
      for (let i = 0; i < indices.length; i += 3) {
        addFace(indices[i], indices[i + 1], indices[i + 2])
      }
    } else {
      const vertCount = verts.length
      for (let i = 0; i < vertCount; i += 3) {
        addFace(i, i + 1, i + 2)
      }
    }

    groups.push({ materialName: matName, faces })
  }

  // Global ground-plane lift (same as STL): zOffset = -minZ + 2.5
  let minZ = Number.POSITIVE_INFINITY
  for (const p of v) minZ = Math.min(minZ, p.z)
  const zOffset = -minZ + 2.5
  for (const p of v) p.z += zOffset

  // Build OBJ text
  let obj = `# HeroForge Export\n`
  obj += `mtllib ${filenameBase}.mtl\n`
  obj += `\n`

  for (const p of v) obj += `v ${p.x} ${p.y} ${p.z}\n`
  obj += `\n`

  for (const t of vt) obj += `vt ${t.u} ${t.v}\n`
  obj += `\n`

  if (includeNormals) {
    for (const n of vn) obj += `vn ${n.x} ${n.y} ${n.z}\n`
    obj += `\n`
  }

  for (const g of groups) {
    obj += `g ${g.materialName}\n`
    obj += `usemtl ${g.materialName}\n`
    for (const f of g.faces) obj += `${f}\n`
    obj += `\n`
  }

  return { obj, groups, v, vt, vn }
}

function buildMTLForMeshes(
  meshes: Mesh[],
  textureDir = 'textures'
): { mtl: string; textures: Array<{ path: string; tex: TextureLike }> } {
  const seen = new Set<string>()
  const texOutputs: Array<{ path: string; tex: TextureLike }> = []

  let mtl = `# HeroForge Export\n\n`

  for (const mesh of meshes) {
    const mat = pickMaterial(mesh)
    if (!mat) continue

    const matName = safeMaterialName(mat, mesh.name || 'mesh')
    if (seen.has(matName)) continue
    seen.add(matName)

    mtl += `newmtl ${matName}\n`
    // Basic defaults (fine for most viewers)
    mtl += `Ka 1.000 1.000 1.000\n`
    mtl += `Kd 1.000 1.000 1.000\n`
    mtl += `Ks 0.000 0.000 0.000\n`
    mtl += `d 1.000\n`
    mtl += `illum 2\n`

    const slots = getTextureSlots(mat)
    for (const { slot, tex } of slots) {
      const baseName = (tex.name || tex.uuid || `${matName}_${slot}`).toString().replace(/[^\w\-]+/g, '_')
      const fileName = `${baseName}.png`
      const path = `${textureDir}/${fileName}`

      // Map only some slots to standard MTL keywords.
      // 'map_Kd' = diffuse/albedo
      // 'map_Bump' = normal/bump (MTL doesn’t standardize normal maps well)
      if (slot === 'map') mtl += `map_Kd ${path}\n`
      else if (slot === 'normalMap') mtl += `map_Bump ${path}\n`
      else if (slot === 'alphaMap') mtl += `map_d ${path}\n`
      else if (slot === 'emissiveMap') mtl += `map_Ke ${path}\n`
      // roughness/metalness/ao aren’t standard in classic MTL; we still export them as files
      // and you can manually wire them in modern tools if needed.

      texOutputs.push({ path, tex })
    }

    mtl += `\n`
  }

  return { mtl, textures: texOutputs }
}

/**
 * Check if a vertex has valid coordinates
 */
function isValidVertex(v: Vertex): boolean {
  const MAX_COORD = 100
  return Math.abs(v.x) < MAX_COORD && Math.abs(v.y) < MAX_COORD && Math.abs(v.z) < MAX_COORD
}

/**
 * Merge mesh geometries into triangles
 */
function collectTriangles(meshes: Mesh[]): Triangle[] {
  const allTriangles: Triangle[] = []
  let skippedTriangles = 0

  for (const mesh of meshes) {
    const { positions, indices } = extractMeshGeometry(mesh)

    const getVertex = (idx: number): Vertex => ({
      x: positions[idx * 3],
      y: positions[idx * 3 + 1],
      z: positions[idx * 3 + 2],
    })

    const addTriangle = (i0: number, i1: number, i2: number): void => {
      const v1 = getVertex(i0)
      const v2 = getVertex(i1)
      const v3 = getVertex(i2)

      if (!isValidVertex(v1) || !isValidVertex(v2) || !isValidVertex(v3)) {
        skippedTriangles++
        return
      }

      allTriangles.push({ v1, v2, v3 })
    }

    if (indices) {
      for (let i = 0; i < indices.length; i += 3) {
        addTriangle(indices[i], indices[i + 1], indices[i + 2])
      }
    } else {
      const vertCount = positions.length / 3
      for (let i = 0; i < vertCount; i += 3) {
        addTriangle(i, i + 1, i + 2)
      }
    }
  }

  if (skippedTriangles > 0) {
    console.warn(`Skipped ${skippedTriangles} triangles with invalid coordinates`)
  }

  return allTriangles
}

/**
 * Transform triangles to match HeroForge orientation and scale
 */
function transformTriangles(triangles: Triangle[], scale: number): Triangle[] {
  // Transform to match HeroForge orientation (Z up)
  for (const tri of triangles) {
    for (const v of [tri.v1, tri.v2, tri.v3]) {
      const oldY = v.y
      const oldZ = v.z
      v.y = -oldZ * scale
      v.z = oldY * scale
      v.x = v.x * scale
    }
  }

  // Move to ground plane
  let minZ = Number.POSITIVE_INFINITY
  for (const tri of triangles) {
    for (const v of [tri.v1, tri.v2, tri.v3]) {
      minZ = Math.min(minZ, v.z)
    }
  }

  const zOffset = -minZ + 2.5
  for (const tri of triangles) {
    for (const v of [tri.v1, tri.v2, tri.v3]) {
      v.z += zOffset
    }
  }

  return triangles
}

/**
 * Calculate face normal
 */
function calculateNormal(v1: Vertex, v2: Vertex, v3: Vertex): Vertex {
  const ax = v2.x - v1.x
  const ay = v2.y - v1.y
  const az = v2.z - v1.z
  const bx = v3.x - v1.x
  const by = v3.y - v1.y
  const bz = v3.z - v1.z

  let nx = ay * bz - az * by
  let ny = az * bx - ax * bz
  let nz = ax * by - ay * bx

  const len = Math.sqrt(nx * nx + ny * ny + nz * nz)
  if (len > 0) {
    nx /= len
    ny /= len
    nz /= len
  }

  return { x: nx, y: ny, z: nz }
}

/**
 * Generate binary STL
 */
function trianglesToSTL(triangles: Triangle[], headerText = 'HeroForge Export'): ArrayBuffer {
  const triangleCount = triangles.length
  const bufferLength = 80 + 4 + triangleCount * 50
  const buffer = new ArrayBuffer(bufferLength)
  const dataView = new DataView(buffer)

  for (let i = 0; i < 80; i++) {
    dataView.setUint8(i, i < headerText.length ? headerText.charCodeAt(i) : 0)
  }

  dataView.setUint32(80, triangleCount, true)

  let offset = 84

  for (const tri of triangles) {
    const normal = calculateNormal(tri.v1, tri.v2, tri.v3)

    dataView.setFloat32(offset, normal.x, true)
    offset += 4
    dataView.setFloat32(offset, normal.y, true)
    offset += 4
    dataView.setFloat32(offset, normal.z, true)
    offset += 4

    dataView.setFloat32(offset, tri.v1.x, true)
    offset += 4
    dataView.setFloat32(offset, tri.v1.y, true)
    offset += 4
    dataView.setFloat32(offset, tri.v1.z, true)
    offset += 4

    dataView.setFloat32(offset, tri.v2.x, true)
    offset += 4
    dataView.setFloat32(offset, tri.v2.y, true)
    offset += 4
    dataView.setFloat32(offset, tri.v2.z, true)
    offset += 4

    dataView.setFloat32(offset, tri.v3.x, true)
    offset += 4
    dataView.setFloat32(offset, tri.v3.y, true)
    offset += 4
    dataView.setFloat32(offset, tri.v3.z, true)
    offset += 4

    dataView.setUint16(offset, 0, true)
    offset += 2
  }

  return buffer
}

/**
 * Download file
 */
function downloadFile(buffer: ArrayBuffer, filename: string): void {
  const blob = new Blob([buffer], { type: 'application/octet-stream' })
  const link = document.createElement('a')
  link.href = URL.createObjectURL(blob)
  link.download = filename
  document.body.appendChild(link)
  link.click()
  document.body.removeChild(link)
  URL.revokeObjectURL(link.href)
}

/**
 * Main export function
 * @param options.filename - Output filename (default: 'heroforge-character')
 * @param options.scale - Scale factor (default: 10 to match HeroForge export size)
 * @param options.separateBase - Export base and character separately in a ZIP (default: true)
 */
export async function exportCharacter(options: ExportOptions = {}): Promise<void> {
  const {
    filename = 'heroforge-character',
    scale = 10,
    separateBase = true,
    format = 'stl',
    includeTextures = true,
    includeNormals = false,
  } = options

  console.log('Starting HeroForge character export...')
  console.log(`  Scale: ${scale}x`)
  console.log(`  Separate base: ${separateBase}`)

  if (typeof CK === 'undefined' || typeof RK === 'undefined') {
    console.error('CK or RK not found. Make sure you are on the HeroForge character page.')
    return
  }

  if (CK.scene) {
    CK.scene.updateMatrixWorld(true)
  }
  const { characterMeshes, baseMeshes } = collectMeshes()
  if (characterMeshes.length === 0 && baseMeshes.length === 0) {
    console.error('No meshes found.')
    return
  }

  // If we’re exporting textures or multiple outputs, prefer ZIP
  const needsZip = separateBase || includeTextures || format === 'obj'

  if (needsZip) {
    const zip = new SimpleZip()

    const addSTL = (meshes: Mesh[], label: string, outName: string) => {
      const tris = collectTriangles(meshes)
      transformTriangles(tris, scale)
      const stl = trianglesToSTL(tris, label)
      zip.addFile(outName, stl)
      console.log(`${label}: ${tris.length} triangles`)
    }

    const addOBJ = async (meshes: Mesh[], outBaseName: string) => {
      const objBase = outBaseName.replace(/\.obj$/i, '')
      const objRes = buildOBJFromMeshes(meshes, `${objBase}`, scale, includeNormals)

      // MTL + textures
      let mtlText: string | null = null
      if (includeTextures) {
        const mtlRes = buildMTLForMeshes(meshes, 'textures')
        mtlText = mtlRes.mtl
        zip.addFile(`${objBase}.mtl`, new TextEncoder().encode(mtlText).buffer)

        // Convert textures to PNGs
        // Deduplicate by path
        const seenPaths = new Set<string>()
        for (const t of mtlRes.textures) {
          if (seenPaths.has(t.path)) continue
          seenPaths.add(t.path)

          const png = await textureToPNG(t.tex)
          if (png) zip.addFile(t.path, png)
        }
      } else {
        // still add a stub MTL if you want; or omit. We'll omit.
      }

      zip.addFile(`${objBase}.obj`, new TextEncoder().encode(objRes.obj).buffer)
    }

    // Character
    if (characterMeshes.length > 0) {
      if (format === 'stl') addSTL(characterMeshes, 'HeroForge Character', `${filename}-character.stl`)
      else await addOBJ(characterMeshes, `${filename}-character`)
    }

    // Base
    if (baseMeshes.length > 0) {
      if (format === 'stl') addSTL(baseMeshes, 'HeroForge Base', `${filename}-base.stl`)
      else await addOBJ(baseMeshes, `${filename}-base`)
    }

    // If format is STL but includeTextures=true, dump textures anyway (no binding)
    if (format === 'stl' && includeTextures) {
      const allMeshes = [...characterMeshes, ...baseMeshes]
      const mtlRes = buildMTLForMeshes(allMeshes, 'textures')
      // Include a “materials” file so you at least see what was found
      zip.addFile(`${filename}-materials.mtl`, new TextEncoder().encode(mtlRes.mtl).buffer)

      const seenPaths = new Set<string>()
      for (const t of mtlRes.textures) {
        if (seenPaths.has(t.path)) continue
        seenPaths.add(t.path)
        const png = await textureToPNG(t.tex)
        if (png) zip.addFile(t.path, png)
      }
    }

    const zipBuffer = zip.generate()
    downloadFile(zipBuffer, `${filename}.zip`)
    console.log(`Export complete! Downloaded ${filename}.zip`)
    return
  }

  // Non-zip single STL path (your old behavior)
  const allMeshes = [...characterMeshes, ...baseMeshes]
  const triangles = collectTriangles(allMeshes)
  transformTriangles(triangles, scale)
  const stlBuffer = trianglesToSTL(triangles)
  downloadFile(stlBuffer, `${filename}.stl`)
}

// Export types for module consumers
export type { ExportOptions, Triangle, Vertex, CollectedMeshes }
