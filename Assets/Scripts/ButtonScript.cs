
using System;
using UnityEngine;
using UnityEngine.SceneManagement;

public class ButtonScript : MonoBehaviour
{
    public GameObject canva;
    public static bool Paused;

    public void Start()
    {
        Cursor.lockState = CursorLockMode.None;
        Cursor.visible = true;
    }

    public void Resume()
    {
        SceneManager.LoadScene(1);
    }
    

    public void Quit()
    {
        Application.Quit();
    }
}
