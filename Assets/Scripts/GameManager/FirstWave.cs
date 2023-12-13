using UnityEngine;
using Object = UnityEngine.Object;

public class FirstWave
{
    private readonly GameObject enemy;
    private readonly GameObject tank;
    private GameObject[] rangeSpawnPoints;
    private GameObject[] meleeSpawnPoints;
    private SecondWave secondWave;
    private float time;
    private bool nextWave;
    public FirstWave(GameObject enemy, GameObject tank, GameObject[] rangeSpawnPoints, GameObject[] meleeSpawnPoints)
    {
        this.tank = tank;
        this.enemy = enemy;
        this.rangeSpawnPoints = rangeSpawnPoints;
        this.meleeSpawnPoints = meleeSpawnPoints;
        nextWave = false;
        time = 3f;
        SpawnEnemies();
    }

    public void CustomUpdate()
    {
        if (time > 0f && !nextWave) CheckForWaveEnd();
        if (nextWave) secondWave.CustomUpdate();
    }

    private void CheckForWaveEnd()
    {
        if (GameObject.FindGameObjectsWithTag("Enemy").Length != 0) return;
        time -= Time.deltaTime;
        if (!(time < 0f)) return;
        nextWave = true;
        NextWave();
    }

    private void NextWave()
    {
        secondWave = new SecondWave(enemy, tank, rangeSpawnPoints, meleeSpawnPoints);
    }
    
    private void SpawnEnemies()
    {
        foreach (GameObject spawn in rangeSpawnPoints)
        {
            Object.Instantiate(enemy, spawn.transform.position, Quaternion.identity);
        }
    }
}

