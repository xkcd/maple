import {useCallback, useEffect, useState} from 'react'

function parse(json: string, fallback: any): any {
  try {
    return JSON.parse(json)
  } catch (err) {
    return fallback
  }
}

export default function useStorage<T>(key: string, initialValue: T): [T, (value: T) => void] {
  const [value, setValue] = useState(parse(localStorage.getItem(key), initialValue) ?? initialValue)

  const handleStorageSet = useCallback((value: T) => {
    localStorage.setItem(key, JSON.stringify(value))
    setValue(value)
  }, [])

  const handleStorageChange = useCallback((ev: StorageEvent) => {
    if (ev.key === key) {
      const value = parse(ev.newValue, initialValue)
      setValue(value)
    }
  }, [])

  useEffect(() => {
    window.addEventListener('storage', handleStorageChange)
    return () => {
      window.removeEventListener('storage', handleStorageChange)
    }
  }, [])

  useEffect(() => {
  }, [value])

  return [value, handleStorageSet]
}
