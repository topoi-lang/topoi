
export interface Span<T> {
    start: number
    end: number
    content: T
}

export interface Identifier {
    value: string
}

export interface MemberAccessExpression {
    object: string
    member: string
}
